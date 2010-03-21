/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.domain.dndi

import scala.util.matching.Regex
import scala.xml.{Node,NodeSeq}


class UntranslatableException(node:scala.xml.Node,e:Throwable) extends Exception("Cant translate node: "+node,e)

/**
 * Converts XML nodes form DNDInsiderCapture into a series os Parts.
 * This involves removing link, converting Break, extrating Keys and Text, and
 * 
 */
object Parser {
  
  private val logger = org.slf4j.LoggerFactory.getLogger("domain")
  
  final val reColonTrim = new Regex("^[:\\s]*(.*?)[;\\s]*$")
  final val reFlexiInt = new Regex("^\\s*([\\+\\-])?\\s*(\\d+)\\s*[\\,\\;]?\\s*$")
  final val reSpaces = new Regex("[\\s\u00a0]+")
  
  /**
   * Symbolic names for the DND Insider icons, we don't need them, this will make the text
   * into Icon(value)
   */
  object IconType extends Enumeration {
    val Separator=Value("X")
    val MeleeBasic=Value("Melee Basic")
    val AreaBasic=Value("Area Basic")
    val CloseBasic=Value("Close Basic")
    val RangeBasic=Value("Range Basic")
    val Range=Value("Range")
    val Melee=Value("Melee")
    val Close=Value("Close")
    val Area = Value("Area")
    val Bullet = Value("*")
    val Unknown=Value("?")
    
    /**
     * A map of names to value
     */
    final val imageDirectory=Map(
      "x.gif"-> Separator,
      "s1.gif"-> CloseBasic,
      "s2.gif"-> MeleeBasic,
      "s3.gif"-> RangeBasic,
      "s4.gif"-> AreaBasic,
      "z1.gif" -> Close,
      "z1a.gif" -> Close,
      "z2.gif" -> Melee,
      "z2a.gif" -> Melee,
      "z3.gif" -> Range,
      "z3a.gif" -> Range,
      "z4.gif" -> Area,
      "z4a.gif" -> Area,
      "bullet.gif" -> Bullet
    ) 
    
    final val iconToImage = Map (
      Separator -> "x.gif",
      CloseBasic -> "s1.gif",
      MeleeBasic -> "s2.gif",
      RangeBasic -> "s3.gif",
      AreaBasic -> "s4.gif",
      Close -> "z1a.gif",
      Melee  -> "z2a.gif",
      Range -> "z3a.gif",
      Area -> "z4a.gif",
      Bullet  -> "bullet.gif",
      Unknown -> "x.gif"
    )
    
    /**
     * Extractor to get a Icon out of a img with the proper image name.
     * Image name is determined form the last slash onwards of the src attribute
     */
    def unapply(node:scala.xml.Node):Option[Value] = {
      if(node.label=="IMG") {
        val url=(node \ "@src").first.toString
        val img=url.substring(url.lastIndexOf("/")+1)
        if(imageDirectory.contains(img.toLowerCase))
          Some(imageDirectory(img.toLowerCase))
        else 
          None
      } else 
        None
    }
  }
  
  /**
   * Extrator to replace the Dice images for text.
   */
  object RechargeDice {
    def unapply(node:scala.xml.Node):Option[Text] = {
      if(node.label=="IMG") {
        val url=(node \ "@src").first.toString
        val img=url.substring(url.lastIndexOf("/")+1)
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
    def unapplySeq(parts:List[Part]):Option[Seq[IconType.Value]] = {
      parts match {
        case Icon(icon1):: Icon(icon2) :: rest => Some(Seq(icon1,icon2))
        case Icon(icon)::rest => Some(Seq(icon))
        case r => Some(Seq())
      } 
      None
    }
  }

  /**
   * Root of all parser tokens
   */
  abstract class Part
  
  /**
   * Line breaks, <BR></BR>
   */
  case class Break() extends Part
  
  /**
   * Text blocks, for #PCDATA
   */
  case class Text(text:String) extends Part {
    /**
     * Since some images and A tags become text we need to merge them
     * and put some whitespace when needed
     */
    def +(that:Text) = {
      val thist = this.text
      val thatt = that.text
      if(thist.length==0) that  // Case for trim that return Text("")
      else if(thatt.length==0) this
      else if(thatt.first.isWhitespace || thist.last.isWhitespace) 
        Text(thist+thatt)
      else
        Text(thist+" "+thatt)
    }
  }
  /**
   * One of the DNDInsider Icons to powers
   */
  case class Icon(itype:IconType.Value) extends Part
  
  /**
   * Text that was surrounded in bold
   */
  case class Key(key:String) extends Part
  
  /**
   * Text that was surrounded in Italic
   */
  case class Emphasis(text:String) extends Part
  
  abstract class BlockElement
  
  case class Block(name:String,parts:List[Part]) extends BlockElement
  
  case class NonBlock(parts:List[Part]) extends BlockElement
  
  case class HeaderBlock(name:String,pairs:List[(String,String)]) extends BlockElement
  
  case class NestedBlocks(name:String,blocks:List[BlockElement]) extends BlockElement
  
  /**
   * Transform a simple node into a Part token. This will lift A tags, B to Keys, 
   * images to icon, recharge dice to text, and attempt several triming to get rid
   * of colons, semi-colons, and other noise after valuable data
   */
  def parseNode(node:scala.xml.Node):Part = {
    
    def processString(str:String): String = {
      str match {
        case reFlexiInt(sign,value) => if("-" == sign) sign+value else value
        case reColonTrim(text) => text
        case nomatch => nomatch
      }
    }
    node match {
      case <BR></BR> => Break()
      case bi @ <B><IMG/></B>  => parseNode(bi.child(0))
      case <B>{text @ _*}</B> => Key(processString(parseToText(text)))
      case <B>{text}</B> => Key(processString(parseToText(text)))  //Go figure (need because o bi above)
      case <A>{text}</A> => Text(processString(parseToText(text)))
      case <I>{i @ _*}</I> => Emphasis(processString(parseToText(i)))
      case RechargeDice(t) => t
      case IconType(itype) => Icon(itype)
      case scala.xml.Text(text) => Text(processString(text))
      case s => 
        logger.debug("Failed to match "+s)
        throw new UntranslatableException(node,null)
    }
  }
  
  /**
   * This methods merges two subsequent Text into a single Text part.
   */
  def mergeText(parts:List[Part]):List[Part]= {
    parts match {
      case (ta:Text)::(tb: Text) :: rest => mergeText((ta+tb)::rest)
      case part :: rest => part :: mergeText(rest)
      case Nil => Nil
    }
  }
  
  
  /**
   * This is an extractor that will get all the text and replace Break for 
   * new line. Returning a single text.
   */
  object SingleTextBreakToNewLine {
    def unapply(parts:List[Part]):Option[String] = {
      Parser.breakToNewLine(parts) match {
        case Text(text)::Nil =>
          Some(text)
        case Nil => Some("")
        case _ => None
      }
    }
  }
  
  /**
   * This is an extractor that will get all the text and replace Break for 
   * new line. Returning a single text.
   */
  object SingleTextBreakToSpace {
    def unapply(parts:List[Part]):Option[String] = {
      Parser.breakToSpace(parts) match {
        case Text(text)::Nil =>
          Some(text)
        case _ => None
      }
    }
  }
  
  /**
   * Replace Break() in descriptin for \n and merge text. Will return a list of parts without the Break
   */
  def breakToNewLine(parts:List[Part]) = mergeText(parts.map(x=> if(x==Break()) Text("\n") else x))
  
  /**
   * Replace Break() in descriptin for \n and merge text. Will return a list of parts without the Break
   */
  def breakToSpace(parts:List[Part]) = mergeText(parts.map(x=> if(x==Break()) Text(" ") else x))

  /**
   * Transform a NodeSeq into a list of parts, with text properly merged.
   */
  def parse(nodes: scala.xml.NodeSeq):List[Part] = {
    if(nodes==null) Nil
    else mergeText((nodes.map(parseNode)).toList)
  }
  
  
  /**
   * Parse to text will remove links and return a text string
   */
  def parseToText(nodes:NodeSeq):String = {
    var s = new StringBuffer("")
    val ss = nodes.map(
      _ match {
        case IconType(icon) =>  "["+icon.toString+"]" 
        case <I>{parts @ _*}</I> => parseToText(parts)
        case <B>{parts @ _*}</B> => parseToText(parts)
        case <A>{parts @ _*}</A> => parseToText(parts)
        case node => node.text 
       }
    )
    ss.mkString("")
  }

  /**
   * Transform a list of paris of type Key,Text and optional breaks into 
   * a list of key,value pairs.
   */
  def partsToPairs(parts:List[Part]):List[(String,String)] = {
    parts match {
      case Key(k)::Text(value)::rest => (k,value):: partsToPairs(rest)
      case Break()::rest => partsToPairs(rest)
      case Nil => Nil
      case s => throw new Exception("List contains unexpected parts: "+s)
    }
  }

  
  private def flattenSpans(xml:scala.xml.NodeSeq):List[(String,String)] = {
    var l:List[(String,String)]=Nil 
    
    l = (for(span<- xml \\ "SPAN") yield {
      ( (span \ "@class").first.toString, span.first.child(0).toString.trim)
    }).toList
    ("name",(xml\"#PCDATA").toString.trim):: l
  }

  
  private final val blockElements = Set("BLOCKQUOTE","P","H2","SPAN")
  
  private def elementClassAttr(node:Node) = node.label +"#" + (if((node \ "@class").isEmpty) "" else (node \ "@class")(0).text)
  
  /**
   * This is a set of partial functions to be applied in order trying to convert the
   * XML into a standar format. It removes some of the oddities in the DND Insider site.
   */
  private val blockStrategies:List[(String,PartialFunction[Node,BlockElement])] = List(
    ("H1 to HeaderBlock",{
      case node @ <H1>{_*}</H1> => HeaderBlock(elementClassAttr(node),flattenSpans(node))
    }),
    ("Misplaced Publish message",{
       case span @ <SPAN>{_*}</SPAN> if((span \\ "P" \ "I").text.startsWith("First"))=>
         val l = parse(span.child.filter(n => n.label != "P"))
         logger.warn("Removing misplaced publishing information: "+(span \\ "P"))
         Block(elementClassAttr(span),l)
    }),
    ("Power Span",{
      case span @ <SPAN>{first,_*}</SPAN> if((span \ "@class").text == "power" && first.label == "H1")=>
        NestedBlocks(elementClassAttr(span),parseBlockElements(span.child,true))
    }),
    ("Block Level Elements", {
      case node if(blockElements.contains(node.label)) => Block(elementClassAttr(node),parse(node.child))
    }),
    ("NonBlock",{
      case bi @ <B><IMG/></B>  => NonBlock(parse(bi.child))
      case <B>{child @ _* }</B> => NonBlock(List(Key(parseToText(child))))
      case <BR/> => NonBlock(List(Break()))
      case scala.xml.Text(txt) => NonBlock(List(Text(txt))) 
      case s => NonBlock(parse(s))
    })
  )
  
  
  /**
   * 
   */
  def parseBlockElement(node:Node,strict:Boolean):BlockElement = {
    for((name,rule) <- blockStrategies) {
		if(rule.isDefinedAt(node)) {
	      logger.debug("Applying rule: '{}' to: {}",name,node)
	      val ret =  try { 
	        rule(node)
	      } catch {
	      	case e if(strict) =>
	      	  logger.warn("Failed to apply rule: '{}' to: {}",name,node)
	      	  throw new UntranslatableException(node,e)
	      	case e => 
	      	  logger.warn("Failed to apply rule '{}' will skip reason",name,e)
	      	  null
	      }
	   	  if(ret!=null) return ret
	    }
	}
    if(strict) throw new UntranslatableException(node,null)
    else {
      logger.warn("Ignoring, since no rule will convert : {}",node)
      null
    }
  }
  
  def parseBlockElements(nodes:NodeSeq,strict:Boolean):List[BlockElement] = {
    nodes.map(node=>try { 
      parseBlockElement(node,strict) 
    } catch { 
      case e =>
        if(strict) throw e
        else null
    }).filter(x => x!=null).toList
  }
}
