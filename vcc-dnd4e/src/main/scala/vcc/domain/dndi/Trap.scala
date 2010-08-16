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
package vcc.domain.dndi

import vcc.infra.text._
import util.matching.Regex
import vcc.domain.dndi.Parser.{HeaderBlock, Icon, Break, Emphasis, Key, NonBlock, Text, Block, BlockElement}
import collection.Seq
import java.lang.String

/**
 *  Represents the capture DNDI Trap
 */
class Trap(val id: Int, var attributes: Map[String, String], val sections: List[TrapSection])
        extends DNDIObject with StatBlockDataSource {
  final val clazz = "trap"

  def extractGroup(group: String): Seq[StatBlockDataSource] = {
    if(group.toUpperCase() == "SECTIONS") sections.toSeq
    else Seq()
  }

  def extract(string: String): Option[String] = apply(string)
}

case class TrapSection(header: String, text: StyledText) extends StatBlockDataSource {

  def extractGroup(group: String): Seq[StatBlockDataSource] = Seq()

  def extract(key: String): Option[String] = wrapInOption(key.toUpperCase() match {
      case "HEADER" => header
      case _ => null
    })

  override def extractRawXHTML(key: String):Option[StyledText] = wrapInOption(key.toUpperCase() match {
    case "TEXT" => text
    case _ => null
  })
}

/**
 * TrapReader is a TokenStream processor that will load a Trap from the Token Stream.
 */
class TrapReader(val id: Int) {
  private var attributes = Map[String, String](
    "NAME" -> "Unknown",
    "HP" -> "9999",
    "AC" -> "99",
    "REFLEX" -> "99",
    "WILL" -> "99",
    "FORTITUDE" -> "99",
    "ROLE" -> "Unspecified",
    "XP" -> "100",
    "INITIATIVE" -> "-99",                                          
    "LEVEL" -> "1"
    )

  private var secs: List[TrapSection] = Nil

  final val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")

  /**
   * Normalizes header information, will remove misformated level and xp information. In these cases will default to
   * initial values.
   */
  private def normalizeTitle(l: List[(String, String)]): List[(String, String)] = {
    l match {
      case ("xp", this.reXP(xp)) :: rest => ("xp", xp) :: normalizeTitle(rest)
      case ("xp", ignore) :: rest => normalizeTitle(rest)
      case ("level", this.reLevel(lvl, role)) :: rest => ("level", lvl) :: ("role", role) :: normalizeTitle(rest)
      case ("level", ignore) :: rest => normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }


  private[dndi] def processSection(stream: TokenStream[BlockElement]): TrapSection = {
    val textBuilder = new TextBuilder()

    val sectionName = stream.head match {
      case Block("SPAN#trapblocktitle", Text(name) :: Nil) =>
        stream.advance()
        name
      case _ =>
        throw new UnexpectedBlockElementException("SPAN trapblocktitle expected", stream.head)
    }
    while (stream.head match {
      case blk@Block("SPAN#trapblockbody", parts) =>
        textBuilder.append(TextBlock("SPAN", "trapblockbody", ParserTextTranslator.partsToStyledText(parts): _*))
        true
      case _ => false
    }) {
      stream.advance()
    }
    TrapSection(sectionName, textBuilder.getDocument())
  }

  private def oneBlockSection(tag: String, clazz: String, stream: TokenStream[BlockElement]): TrapSection = {
    val textBuilder = new TextBuilder()
    stream.head match {
      case blk@Block(name, parts) if (name == tag + "#" + clazz) =>
        textBuilder.append(TextBlock(tag, clazz, ParserTextTranslator.partsToStyledText(parts): _*))
      case s =>
        throw new UnexpectedBlockElementException("Expect block with name '" + tag + "#" + clazz + "'", s)
    }
    stream.advance()
    TrapSection(null, textBuilder.getDocument())
  }

  private[dndi] def processHeader(stream: TokenStream[BlockElement]) {
    val headMap: Map[String, String] = stream.head match {
      case HeaderBlock("H1#trap", values) => normalizeTitle(values).toMap[String, String]
      case s => throw new UnexpectedBlockElementException("Expected H1 block", s)
    }
    for (key <- List("xp", "name", "level", "role", "type")) {
      if (headMap.isDefinedAt(key)) attributes = attributes + (key.toUpperCase -> headMap(key))
    }
    stream.advance()
  }

  def process(blocks: List[BlockElement]): Trap = {
    val stream = new TokenStream[BlockElement](blocks.filterNot(x => x.isInstanceOf[NonBlock]))

    //Safe guard for some empty stream
    if(!stream.advance) false
    
    processHeader(stream)
    while (stream.head match {
      case Block("SPAN#trapblocktitle", ignore) =>
        val sec = processSection(stream)
        secs = sec :: secs
        true
      case Block("P#flavor", parts) =>
        secs = oneBlockSection("P", "flavor", stream) :: secs
        true
      case Block("SPAN#traplead", parts) =>
        // Check for special initiative line
        parts.map(p => p.transform(Parser.TrimProcess)) match {
          case Key("Initiative") :: Text(value) :: rest =>
            attributes = attributes + ("INITIATIVE" -> value)
          case _ => // Nothing
        }
        secs = oneBlockSection("SPAN", "traplead", stream) :: secs
        true
      case _ => false
    }) {

    }
    new Trap(id, attributes, secs.reverse)
  }
}