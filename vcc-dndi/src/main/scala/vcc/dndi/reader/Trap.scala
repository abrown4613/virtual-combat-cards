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

import vcc.infra.text._
import util.matching.Regex
import vcc.dndi.reader.Parser._
import xml.NodeSeq
import vcc.infra.xtemplate.TemplateDataSource
import org.exnebula.iteratee._
import CompendiumCombatantEntityMapper._
/**
 * Represents the capture DNDI Trap
 */
class Trap(val id: Int, var attributes: Map[String, String], val sections: List[TrapSection])
  extends DNDIObject with TemplateDataSource {
  final val clazz = "trap"

  def templateVariable(key: String): Option[String] = apply(key)

  def templateInlineXML(key: String): NodeSeq = null

  def templateGroup(key: String): List[TemplateDataSource] = {
    if (key.toLowerCase == "sections") sections
    else Nil
  }
}

case class TrapSection(header: String, text: StyledText) extends TemplateDataSource {

  /**
   * Helper function to make null into None, and other values to Some
   * @param tgt What should be wrapped in an option
   */
  protected def wrapInOption[T](tgt: T): Option[T] = if (tgt != null) Some(tgt) else None

  def templateGroup(group: String): List[TemplateDataSource] = Nil

  def templateVariable(key: String): Option[String] = wrapInOption(key.toLowerCase match {
    case "header" => header
    case _ => null
  })

  def templateInlineXML(key: String): NodeSeq = key.toLowerCase match {
    case "text" => text.toXHTML
    case _ => Nil
  }

  def isEmpty = (header == null) && (text.blocks.isEmpty)
}

/**
 * TrapReader is a TokenStream processor that will load a Trap from the Token Stream.
 */
class TrapReader(val id: Int) extends DNDIObjectReader[Trap] {
  private val attributes = Map[String, String](
    "name" -> "Unknown",
    "hp" -> "9999",
    "ac" -> "99",
    "reflex" -> "99",
    "will" -> "99",
    "fortitude" -> "99",
    "role" -> "Unspecified",
    "xp" -> "100",
    "initiative" -> "-99",
    "level" -> "1"
  )

  final val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")

  /**
   * Normalizes header information, will remove mis-formatted level and xp information. In these cases will default to
   * initial values.
   */
  private def normalizeTitle(l: List[(String, String)]): List[(String, String)] = {
    l match {
      case ("xp", reXP(xp)) :: rest => ("xp", xp) :: normalizeTitle(rest)
      case ("xp", ignore) :: rest => normalizeTitle(rest)
      case ("level", reLevel(lvl, role)) :: rest => ("level", lvl) ::("role", role) :: normalizeTitle(rest)
      case ("level", ignore) :: rest => normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }

  private[dndi] val readTrapBlockTitle = matchConsumer("SPAN trapblocktitle") {
    case Block("SPAN#trapblocktitle", Text(name) :: Nil) => name
    case Block("SPAN#trapblocktitle", Nil) => null
  }

  private[dndi] val ignoreEmptySection: Consumer[BlockElement, Unit] = dropWhile(block => block == Block("SPAN#trapblocktitle", Nil))

  private[dndi] val readTrapBlockBody = matchConsumer("SPAN trapblockbody") {
    case Block("SPAN#trapblockbody", parts) =>
      TextBlock("SPAN", "trapblockbody", ParserTextTranslator.partsToStyledText(parts): _*)
  }

  private[dndi] val readFlavor = readOneBlockSection("P", "flavor")

  private[dndi] val readDescription = readOneBlockSection("SPAN", "traplead")

  private[dndi] val readComment = matchConsumer("SPAN trapblocktitle") {
    case Block(tagClass, Text(comment) :: Nil) if (tagClass.startsWith("P#")) => comment
    case Block(tagClass, Emphasis(comment) :: Nil) if (tagClass.startsWith("P#")) => comment
  }

  private[dndi] val readInitiative = matchConsumer("SPAN traplead with initiative") {
    case block@Block("SPAN#traplead", Key("Initiative") :: Text(value) :: rest) =>
      val textBuilder = new TextBuilder
      textBuilder.append(TextBlock("SPAN", "traplead", ParserTextTranslator.partsToStyledText(block.parts): _*))
      (Parser.TrimProcess(value.trim), TrapSection(null, textBuilder.getDocument()))
  }

  private def dropWhile[I](p: I => Boolean) = new Consumer[I, Unit] {
    def consume(input: Input[I]): ConsumerState[I, Unit] = input match {
      case EOF => Done((), EOF)
      case Empty => Continue(this)
      case Chunk(c) if(p(c)) => Continue(this)
      case Chunk(c) => Done((),input)
    }
  }

  private def readOneBlockSection(tag: String, clazz: String) = matchConsumer(tag + " " + clazz) {
    case Block(tagClass, parts) if (tagClass == tag + "#" + clazz) =>
      val textBuilder = new TextBuilder
      textBuilder.append(TextBlock(tag, clazz, ParserTextTranslator.partsToStyledText(parts): _*))
      TrapSection(null, textBuilder.getDocument())
  }

  private def matchConsumer[T](expected: String)(matcher: PartialFunction[BlockElement, T]) = new Consumer[BlockElement, T] {
    def consume(input: Input[BlockElement]): ConsumerState[BlockElement, T] = input match {
      case Chunk(c) if (matcher.isDefinedAt(c)) => Done(matcher(c), Empty)
      case Chunk(c) => Error(new UnexpectedBlockElementException(expected + " expected", c), input)
      case EOF => Error(new UnexpectedBlockElementException(expected + " expected got EOF", null), EOF)
      case Empty => Continue(this)
    }
  }

  private[dndi] val readSection = for {
    name <- readTrapBlockTitle
    texts <- repeat(readTrapBlockBody)
  } yield TrapSection(name, StyledText(texts))

  private[dndi] val readSections = for {
    sections <- repeat(readSection)
  } yield (sections.filterNot(_.isEmpty))

  private[dndi] val readHeader = matchConsumer[Map[String, String]]("Trap header") {
    case HeaderBlock("H1#trap", values) => normalizeTitle(values).toMap[String, String]
  }

  private[dndi] val readTrapOld = for {
    headMap <- readHeader
    flavor <- readFlavor
    desc <- readDescription
    sec1 <- repeat(readSection)
    inits <- repeat(readInitiative)
    sec2 <- repeat(readSection)
    comment <- readComment
  } yield {
    val init = inits.headOption
    val initSec = init.map(_._2).toList
    val nAttributes = updateAttributes(attributes, headMap) + ("comment" -> comment) ++ init.map("initiative" -> _._1)
    new Trap(id, normalizeCompendiumNames(nAttributes), List(flavor, desc) ++ sec1 ++ initSec ++ sec2)
  }

  private val readHeaderNew = matchConsumer("new head block ") {
    case HeaderBlock("H1#thHead", values) => normalizeTitle(values).toMap[String, String]
  }

  private[dndi] val readTrapNew = for {
    headMap <- readHeaderNew
    _ <- dropWhile[BlockElement](x => true)
  } yield {
    new Trap(id, normalizeCompendiumNames(updateAttributes(attributes, headMap).updated("type", "new")), Nil)
  }

  def process(blocks: List[BlockElement]): Trap = {
    (readTrapNew orElse readTrapOld).consumeAll(blocks.filterNot(_.isInstanceOf[NonBlock])) match {
      case (Right(trap),Nil) => trap
      case (Right(trap),rest) => throw new UnexpectedBlockElementException("Unconsumed block: ", rest.head)
      case (Left(error),rest) => throw error
    }
  }

  private def updateAttributes(attribute: Map[String,String], newValue: Map[String,String]): Map[String,String] =
    List("xp", "name", "level", "role", "type").
      foldLeft(attribute)((as,key)=> if(newValue.isDefinedAt(key)) as.updated(key, newValue(key)) else as)

}