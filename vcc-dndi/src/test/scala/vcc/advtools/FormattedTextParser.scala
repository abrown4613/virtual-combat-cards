/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.advtools

import scala.util.parsing.combinator._
import vcc.dndi.common.FormattedText._

object FormattedTextParser {

  def parseBlock(input:String):Option[Block] = {
    val parser = new FormattedTextParser
    val r = parser.parseAll(parser.block, input)
    if (r.isEmpty) None else Some(r.get)
  }

}

private class FormattedTextParser extends RegexParsers {
  override val whiteSpace = "".r

  def block: Parser[Block] = repsep(line, lineBreak) ~ opt(lineBreak) ^^ {
    case x ~ _ =>
      Block(x)
  }

  def line: Parser[Line] = indent ~ part ~ rep(part) ^^ {
    case i ~ x ~ xs => Line(i.length, x :: xs)
  }

  private def part: Parser[Part] = italic | normal

  private def italic: Parser[Part] = "_" ~ text ~ "_" ^^ {
    case _ ~ x ~ _ => Italic(x)
  }

  private def normal: Parser[Part] = text ^^ {
    x => Normal(x)
  }

  private def text: Parser[String] = """([^_\n\r\t]+)""".r

  private def lineBreak: Parser[String] = """([\n\r]+)""".r

  private def indent: Parser[String] = """(\t*)""".r

}