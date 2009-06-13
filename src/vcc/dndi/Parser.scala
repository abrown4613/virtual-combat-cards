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
package vcc.dndi

class UntranslatableException(node:scala.xml.Node) extends Exception("Cant translate node: "+node)

/**
 * Converts XML nodes form DNDInsiderCapture into a series os Parts.
 * This involves removing link, converting Break, extrating Keys and Text, and
 * 
 */
object Parser {
  
  final val reColonTrim=new scala.util.matching.Regex("^[:\\s]*(.*?)[;\\s]*$")
  final val reFlexiInt=new scala.util.matching.Regex("^\\s*([\\+\\-])?\\s*(\\d+)\\s*[\\,\\;]?\\s*$")
  
  /**
   * Symbolic names for the DND Insider icons, we don't need them, this will make the text
   * into Icon(value)
   */
  object IconType extends Enumeration {
    val Separator=Value("X")
    val MeleeBasic=Value("Melee Basic")
    val Range=Value("Range")
    val Melee=Value("Melee")
    val Close=Value("Close")
    val RangeBasic=Value("Range Basic")
    val Area = Value("Area")
    val Unknown=Value("?")
    
    /**
     * A map of names to value
     */
    final val imageDirectory=Map(
      "x.gif"-> Separator,
      "s3.gif"-> RangeBasic,
      "s2.gif"-> MeleeBasic,
      "z4a.gif" -> Area,
      "z3a.gif" -> Range,
      "z2a.gif" -> Melee,
      "z1a.gif" -> Close
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
      if(this.text.length==0) that  // Case for trim that return Text("") 
      else if(that.text(0).isLetter || that.text(0).isDigit) 
        Text(this.text+" "+that.text)
      else
        Text(this.text+that.text)
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
  
  /**
   * Transform a simple node into a Part token. This will lift A tags, B to Keys, 
   * images to icon, recharge dice to text, and attempt several triming to get rid
   * of colons, semi-colons, and other noise after valuable data
   */
  def parseNode(node:scala.xml.Node):Part = {
    node match {
      case <BR></BR> => Break()
      case <B>{text}</B> => Key(text.toString.trim)
      case <A>{text}</A> => Text(text.toString.trim)
      case <I>{text}</I> => Emphasis(text.toString.trim)
      case RechargeDice(t) => t
      case IconType(itype) => Icon(itype)
      case scala.xml.Text(reFlexiInt(sign,value)) => if(sign!=null )Text(sign+value) else Text(value)
      case scala.xml.Text(reColonTrim(text)) => if(text!=null) Text(text) else Text("")
      case scala.xml.Text(text) => Text(text.trim)
      case s => throw new UntranslatableException(node)
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
   * Transform a NodeSeq into a list of parts, with text properly merged.
   */
  def parse(nodes: scala.xml.NodeSeq):List[Part] = {
    if(nodes==null) Nil
    else mergeText((nodes.map(parseNode)).toList)
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
}
