/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dndi.common

import scala.util.parsing.combinator._
import vcc.infra.text._

object MarkdownStyledTextParser {
  def parse(input: String): Either[String, StyledText] = {
    val parser = new MarkdownStyledTextParser()
    parser.parseAll(parser.styledText, input) match {
      case parser.Success(result: StyledText, _) => Right(result)
      case failure: parser.NoSuccess => Left(failure.msg)
    }
  }
}

private class MarkdownStyledTextParser() extends RegexParsers {
  override val whiteSpace = "".r

  private def text: Parser[String] = """([^_{}\*\n\r\t\[]+)""".r

  private def lineBreak: Parser[String] = """([\n\r]+)""".r

  private def xmlIdentifier: Parser[String] = """\w+""".r

  private def styledText: Parser[StyledText] = repsep(block, lineBreak) ~ opt(lineBreak) ^^ {
    case blocks ~ lineBreak =>
      if (lineBreak.isDefined)
        StyledText(blocks.init ++ blocks.lastOption.map(b => b.copy(segments = b.segments ++ List(LineBreak))).toList)
      else
        StyledText(blocks)
  }

  private def block: Parser[TextBlock] = tagClass ~ repsep(segments, lineBreak) ^^ {
    case blockType ~ lines => TextBlock(blockType._1, blockType._2, mergeLines(lines))
  }

  private def tagClass: Parser[(String, String)] = "[" ~ opt(tagName) ~ opt(xmlIdentifier) ~ "]" ^^ {
    case _ ~ tag ~ aClass ~ _ => (tag.getOrElse("P"), aClass.getOrElse(null))
  }

  private def tagName: Parser[String] = xmlIdentifier ~ ":" ^^ {
    case t ~ _ => t
  }

  private def segments = part ~ rep(part) ^^ {
    case x ~ xs => x :: xs
  }

  private def part: Parser[Segment] = normal | bold | italic | image

  private def normal: Parser[Segment] = text ^^ {
    x => TextSegment(x)
  }

  private def bold: Parser[Segment] = "*" ~ text ~ "*" ^^ {
    case _ ~ x ~ _ => TextSegment.makeBold(x)
  }

  private def italic: Parser[Segment] = "_" ~ text ~ "_" ^^ {
    case _ ~ x ~ _ => TextSegment.makeItalic(x)
  }

  private def image: Parser[Segment] = "{" ~ text ~ "}" ^^ {
    case _ ~ src ~ _ => InlineImage(src)
  }

  private def mergeLines(list: List[List[Segment]]): List[Segment] = list match {
    case x1 :: x2 :: rest => x1 ++ List(LineBreak) ++ mergeLines(x2 :: rest)
    case x1 :: Nil => x1
    case Nil => Nil
  }
}