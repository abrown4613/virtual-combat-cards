/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.text

import scala.xml._

object Style extends Enumeration {
  val Bold = Value("Bold")
  val Italic = Value("Italic")
  val None = Value("None")
}

/**
 * This class represents a series of blocks of styled text. I can be turned into both XML and XHTML for rendering
 * purposes.
 */
case class StyledText(blocks: List[TextBlock]) {
  def toXML(): Node = <styledText>{blocks.map(_.toXML)}</styledText>
  def toXHTML(): NodeSeq = blocks.map(_.toXHTML)
}

object StyledText {

  def fromXML(node:Node):StyledText = {
    StyledText((node \ "block").map(TextBlock.fromXML(_)).toList)
  }

}

case class TextBlock(tag: String, clazz: String, segments: List[Segment]) {
  def toXHTML(): Node = {
    val attributes = {
      if (clazz != null) new UnprefixedAttribute("class", clazz, scala.xml.Null)
      else scala.xml.Null
    }
    new Elem(null, tag.toLowerCase, attributes, scala.xml.TopScope, segments.map(_.toXHTML): _*)
  }

  def toXML(): Node = <block tag={tag} class={clazz}>{segments.map(_.toXML)}</block>
}

object TextBlock {
  def apply(tag: String, clazz: String, segs: Segment*) = new TextBlock(tag, clazz, segs.toList)

  def fromXML(node:Node):TextBlock = {
    val tag = (node \ "@tag")(0).text
    val clazz = (node \ "@class")(0).text
    TextBlock(tag,clazz, node.child.map(extractSegment(_)).filter(_ != null) : _*)
  }

  def extractSegment(node:Node):Segment = {
    node match {
      case <text>{data}</text> =>
        TextSegment.fromXML(node)
      case <break /> =>
        LineBreak
      case WhiteSpace(ws) =>
        null //Ignored
      case _ =>
        throw new Exception("Unknown node: "+node)
    }
  }

  object WhiteSpace {
    private val space = """^[\s\n\r\t]*$""".r
    def unapply(node:Node):Option[String] = {
      node match {
        case Text(data) if(!space.findAllIn(data).isEmpty) => Some(data)
        case _ => None
      }
    }
  }
}

abstract class Segment {
  def toXML(): Node
  def toXHTML():Node
}

case class InlineImage(src:String) extends Segment {
  def toXML() = <image url={src} />
  val toXHTML = <img src={src} />
}

case class TextSegment(style: Style.Value, text: String) extends Segment {
  def toXML() = <text style={style.toString}>{text}</text>

  def toXHTML() = style match {
    case Style.Bold => <b>{text}</b>
    case Style.None => scala.xml.Text(text)
    case Style.Italic => <i>{text}</i>
  }
}

case object LineBreak extends Segment {
  def toXML() = <break/>
  val toXHTML = <br/>
}

object TextSegment {

  def apply(text: String) = new TextSegment(Style.None, text)

  def makeBold(text:String) = new TextSegment(Style.Bold, text)

  def makeItalic(text:String) = new TextSegment(Style.Italic, text)

  def fromXML(node:Node):TextSegment = {
    val style = Style.withName((node \ "@style")(0).text)
    TextSegment(style, node.child(0).text)
  }
}