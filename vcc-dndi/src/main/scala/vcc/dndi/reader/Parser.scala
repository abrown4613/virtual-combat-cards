/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package vcc.dndi.reader

import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}
import java.lang.String
import vcc.dndi.reader.Parser.BlockElement

class UntranslatableException(node: Node, e: Throwable) extends Exception("Cant translate node: " + node, e)

class UnexpectedBlockElementException(msg: String, block: BlockElement) extends Exception(msg + " unexpected block: " + block)

/**
 * Converts XML nodes form DNDInsiderCapture into a series os Parts.
 * This involves removing link, converting Break, extracting Keys and Text, and *
 */
object Parser {
  private val logger = org.slf4j.LoggerFactory.getLogger("domain")

  val reColonTrim = new Regex("^[:\\s]*(.*?)[;\\s]*$")
  val reFlexiInt = new Regex("^\\s*([\\+\\-])?\\s*(\\d+)\\s*[\\,\\;]?\\s*$")
  val reSpaces = new Regex("[\\s\u00a0]+")

  /**
   * Symbolic names for the DND Insider icons, we don't need them, this will make the text
   * into Icon(value)
   */
  object IconType extends Enumeration {
    val Separator = Value("X")
    val MeleeBasic = Value("Melee Basic")
    val AreaBasic = Value("Area Basic")
    val CloseBasic = Value("Close Basic")
    val RangeBasic = Value("Range Basic")
    val Range = Value("Range")
    val Melee = Value("Melee")
    val Close = Value("Close")
    val Area = Value("Area")
    val Aura = Value("Aura")
    val Bullet = Value("*")
    val Unknown = Value("?")

    /**
     * A map of names to value
     */
    val imageDirectory = Map(
      "x.gif" -> Separator,
      "s1.gif" -> CloseBasic,
      "s2.gif" -> MeleeBasic,
      "s3.gif" -> RangeBasic,
      "s4.gif" -> AreaBasic,
      "z1.gif" -> Close,
      "z1a.gif" -> Close,
      "z2.gif" -> Melee,
      "z2a.gif" -> Melee,
      "z3.gif" -> Range,
      "z3a.gif" -> Range,
      "z4.gif" -> Area,
      "z4a.gif" -> Area,
      "bullet.gif" -> Bullet,
      "aura.png" -> Aura)

    val iconToImage = Map(
      Separator -> "x.gif",
      CloseBasic -> "s1.gif",
      MeleeBasic -> "s2.gif",
      RangeBasic -> "s3.gif",
      AreaBasic -> "s4.gif",
      Close -> "z1a.gif",
      Melee -> "z2a.gif",
      Range -> "z3a.gif",
      Area -> "z4a.gif",
      Bullet -> "bullet.gif",
      Aura -> "aura.png",
      Unknown -> "x.gif")

    val diceImage: Map[Int, String] = (1 to 6).map(d => (d -> d.formatted("%da.gif"))).toMap

    /**
     * Extractor to templateVariable a Icon out of a img with the proper image name.
     * Image name is determined form the last slash onwards of the src attribute
     */
    def unapply(node: scala.xml.Node): Option[Value] = {
      if (node.label == "IMG" && !(node \ "@src").isEmpty) {
        val url = (node \ "@src").head.toString()
        val img = url.substring(url.lastIndexOf("/") + 1)
        if (imageDirectory.contains(img.toLowerCase))
          Some(imageDirectory(img.toLowerCase))
        else
          None
      } else
        None
    }

    def iconAsBasic(icon: IconType.Value):IconType.Value = icon match {
      case Area => AreaBasic
      case Close => CloseBasic
      case Range => RangeBasic
      case Melee => MeleeBasic
    }
  }

  /**
   * Extractor to replace the Dice images for text.
   */
  object RechargeDice {
    def unapply(node: scala.xml.Node): Option[Text] = {
      if (node.label == "IMG") {
        val url = (node \ "@src").head.toString()
        val img = url.substring(url.lastIndexOf("/") + 1)
        img match {
          case "1a.gif" => Some(Text("1"))
          case "2a.gif" => Some(Text("2"))
          case "3a.gif" => Some(Text("3"))
          case "4a.gif" => Some(Text("4"))
          case "5a.gif" => Some(Text("5"))
          case "6a.gif" => Some(Text("6"))
          case s => None
        }
      } else None
    }
  }

  object IconSet {
    def unapplySeq(parts: List[Part]): Option[Seq[IconType.Value]] = {
      parts match {
        case Icon(icon1) :: Icon(icon2) :: rest => Some(Seq(icon1, icon2))
        case Icon(icon) :: rest => Some(Seq(icon))
        case r => Some(Seq())
      }
      None
    }
  }

  /**
   * Root of all parser tokens
   */
  abstract class Part {
    /**
     * Applies transformation function to the text of the object. And returns a new object with the transformed text.
     * Default implementation does nothing.
     * @param tf Transformation Function
     * @return A new part with the transformed function 
     */
    def transform(tf: String => String): Part = this
  }

  /**
   * Line breaks, <BR></BR>
   */
  case object Break extends Part {
    def apply() = this
  }

  /**
   * Text blocks, for #PCDATA
   */
  case class Text(text: String) extends Part {
    /**
     * Since some images and A tags become text we need to merge them
     * and put some whitespace when needed
     */
    def +(that: Text) = {
      val thisText = this.text
      val thatText = that.text
      if (thisText.length == 0) that // Case for trim that return Text("")
      else if (thatText.length == 0) this
      else if (thatText.head.isWhitespace || thisText.last.isWhitespace)
        Text(thisText + thatText)
      else
        Text(thisText + " " + thatText)
    }

    override def transform(tf: (String) => String): Part = {
      Text(tf(text))
    }
  }

  /**
   * One of the DNDInsider Icons to powers
   */
  case class Icon(iconType: IconType.Value) extends Part

  /**
   * Text that was surrounded in bold
   */
  case class Key(key: String) extends Part {
    override def transform(tf: (String) => String): Part = {
      Key(tf(key))
    }
  }

  /**
   * Text that was surrounded in Italic
   */
  case class Emphasis(text: String) extends Part {

    override def transform(tf: (String) => String): Part = {
      Emphasis(tf(text))
    }
  }

  /**
   * Block level Elements
   */
  abstract class BlockElement

  case class Block(name: String, parts: List[Part]) extends BlockElement

  case class NonBlock(parts: List[Part]) extends BlockElement

  case class HeaderBlock(name: String, pairs: List[(String, String)]) extends BlockElement

  case class NestedBlocks(name: String, blocks: List[BlockElement]) extends BlockElement

  case class Cell(clazz: String, content: List[Part])

  case class Table(clazz: String, cells: List[Cell]) extends BlockElement

  object TrimProcess extends ((String) => String) {
    def apply(str: String): String = str match {
      case reFlexiInt(sign, value) => if ("-" == sign) sign + value else value
      case reColonTrim(text) => text
      case noMatch => noMatch
    }
  }

  /**
   * Transform a simple node into a Part token. This will lift A tags, B to Keys, 
   * images to icon, recharge dice to text, and attempt several triming to templateVariable rid
   * of colons, semi-colons, and other noise after valuable data
   */
  def parseNode(node: scala.xml.Node): Part = {

    node match {
      case <BR></BR> => Break()
      case <B>{b @ _*}</B> => Key(parseToText(b))  //Go figure (need because o bi above)
      case <A>{text @ _*}</A> => Text(parseToText(text))
      case <I>{i @ _*}</I> => Emphasis(parseToText(i))
      case RechargeDice(t) => t
      case IconType(itype) => Icon(itype)
      case scala.xml.Text(text) => Text(text)
      case s =>
        logger.debug("Failed to match " + s)
        throw new UntranslatableException(node, null)
    }
  }

  /**
   * This methods merges two subsequent Text into a single Text part.
   */
  def mergeText(parts: List[Part]): List[Part] = {
    parts match {
      case (ta: Text) :: (tb: Text) :: rest => mergeText((ta + tb) :: rest)
      case part :: rest => part :: mergeText(rest)
      case Nil => Nil
    }
  }

  /**
   * This is an extractor that will templateVariable all the text and replace Break for
   * new line. Returning a single text.
   */
  object SingleTextBreakToNewLine {
    def unapply(parts: List[Part]): Option[String] = {
      Parser.breakToNewLine(parts) match {
        case Text(text) :: Nil =>
          Some(text)
        case Nil => Some("")
        case _ => None
      }
    }
  }

  /**
   * This is an extractor that will templateVariable all the text and replace Break for
   * new line. Returning a single text.
   */
  object SingleTextBreakToSpace {
    def unapply(parts: List[Part]): Option[String] = {
      Parser.breakToSpace(parts) match {
        case Text(text) :: Nil =>
          Some(text)
        case _ => None
      }
    }
  }

  /**
   * Replace Break() in description for \n and merge text. Will return a list of parts without the Break
   */
  def breakToNewLine(parts: List[Part]) = mergeText(parts.map(x => if (x == Break()) Text("\n") else x))

  /**
   * Replace Break() in description for \n and merge text. Will return a list of parts without the Break
   */
  def breakToSpace(parts: List[Part]) = mergeText(parts.map(x => if (x == Break()) Text(" ") else x))

  /**
   * Transform a NodeSeq into a list of parts, with text properly merged.
   */
  def parse(nodes: scala.xml.NodeSeq): List[Part] = {
    if (nodes == null) Nil
    else mergeText((nodes.map(parseNode)).toList)
  }

  /**
   * Parse to text will remove links and return a text string
   */
  def parseToText(nodes: NodeSeq): String = {
    val ss = nodes.map(
      _ match {
        case IconType(icon) =>  "["+icon.toString+"]"
        case <I>{parts @ _*}</I> => parseToText(parts)
        case <B>{parts @ _*}</B> => parseToText(parts)
        case <A>{parts @ _*}</A> => parseToText(parts)
        case <BR/> => "\n"
        case node => node.text
      }
    )
    ss.mkString("")
  }

  /**
   * Transform a list of pairs of type Key,Text and optional breaks into
   * a list of key,value pairs.
   * @param parts   List to parse
   */
  def partsToPairs(parts: List[Part]): List[(String, String)] = {
    parts match {
      case Text("") :: rest => partsToPairs(rest)
      case Key(k) :: Text(value) :: rest => (k, value) :: partsToPairs(rest)
      case Break :: rest => partsToPairs(rest)
      case Nil => Nil
      case s =>
        logger.warn("Trimming tail block: {}", parts)
        Nil
    }
  }

  private def flattenSpans(xml: scala.xml.NodeSeq): List[(String, String)] = {
    var l: List[(String, String)] = Nil

    l = (for (span <- xml \\ "SPAN") yield {
      ((span \ "@class").head.toString(), span.head.child(0).toString().trim)
    }).toList
    ("name", (xml \ "#PCDATA").toString().trim) :: l
  }

  private val blockElements = Set("BLOCKQUOTE", "P", "H2", "SPAN", "TD")

  private def elementClassAttr(node: Node) = node.label + "#" + elementClass(node, "")

  private def elementClass(node: Node, default: String) = (if ((node \ "@class").isEmpty) default else (node \ "@class")(0).text)

  /**
   * This is a set of partial functions to be applied in order trying to convert the
   * XML into a standard format. It removes some of the oddities in the DND Insider site.
   */
  private val blockStrategies: List[(String, PartialFunction[Node, BlockElement])] = List(
    ("H1 to HeaderBlock", {
      case node @ <H1>{ _* }</H1> => HeaderBlock(elementClassAttr(node), flattenSpans(node))
    }),
  ("Misplaced Publish message", {
    case span @ <SPAN>{ _* }</SPAN> if ((span \\ "P" \ "I").text.startsWith ("First") ) =>
      val l = parse(span.child.filter(n => n.label != "P"))
      logger.warn("Removing misplaced publishing information: " + (span \\ "P"))
      Block(elementClassAttr(span), l)
  }),
  ("Power Span", {
    case span @ <SPAN>{ first, _* }</SPAN > if ((span \ "@class").text == "power" && first.label == "H1") =>
      NestedBlocks(elementClassAttr(span), parseBlockElements(span.child))
  }),
    ("Block Level Elements", {
      case node if (blockElements.contains (node.label) ) => Block (elementClassAttr (node), parse (node.child) )
  }),
  ("Tabular Stats", {
    case table @ <TABLE>{ _* }</TABLE> =>
      val tds = (table \\ "TD").map(node => Cell(elementClass(node, null), parse(node.child)))
      Table(elementClass(table, ""), tds.toList)
  }),
  ("NonBlock", {
    case <B>{child @ _*}</B> => NonBlock(List(Key(parseToText(child))))
    case <BR/> => NonBlock (List (Break () ) )
    case scala.xml.Text (txt) => NonBlock (List (Text (txt) ) )
    case s => NonBlock (parse (s) )
  })
  )

  @throws(classOf[UntranslatableException])
  def parseBlockElement(node: Node): BlockElement = {
    val ruleOption = blockStrategies.find(rp => rp._2.isDefinedAt(node))
    if (ruleOption.isDefined) {
      val (name,rule) = ruleOption.get
      logger.debug("Applying rule: '{}' to: {}", Array(name, node))
      try {
        rule(node)
      } catch {
        case e: Throwable =>
          logger.warn("Failed to apply rule: '{}' to: {}", Array(name, node))
          throw new UntranslatableException(node, e)
      }
    } else {
      throw new UntranslatableException(node, null)
    }
  }

  def parseBlockElements(nodes: NodeSeq): List[BlockElement] =
    nodes.map(node => parseBlockElement(node)).toList
}