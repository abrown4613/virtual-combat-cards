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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.infra.text._
import vcc.dndi.reader.Parser.IconType
import vcc.advtools.Monster.{BasicAttack, NormalAttack}

class PowerDescriptionFormatterTest extends SpecificationWithJUnit {

  private val tagClass = "flavorIndent"
  private val tagClass1 = "flavorIndent1"
  private val tagClass2 = "flavorIndent2"

  "format a Block text" in {
    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "Melee 3; +11 vs. AC"),
      makeEntry(tagClass, "Hit: ", "1d8+4 slowed EOT"))
    val r = FormattedTextParser.parseBlock("_Attack: _Melee 3; +11 vs. AC\n_Hit: _1d8+4 slowed EOT").get
    PowerDescriptionFormatter.formatAttack(r) must_== block
  }

  "format with several indent" in {
    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "Melee 3; +11 vs. AC"),
      makeEntry(tagClass1, "Hit: ", "1d8+4 slowed EOT"),
      makeEntry(tagClass2, "More: ", "More indent"))
    val r = FormattedTextParser.parseBlock("_Attack: _Melee 3; +11 vs. AC\n\t_Hit: _1d8+4 slowed EOT\n\t\t_More: _More indent").get
    PowerDescriptionFormatter.formatAttack(r) must_== block
  }

  "extract icon correctly" in {
    import IconType._
    val expected = Map[String, IconType.Value](
      "Melee" -> Melee,
      "Ranged" -> Range,
      "Close blast" -> Close,
      "Close bUrst" -> Close,
      "area burst" -> Area,
      "area" -> Area)

    for ((text, icon) <- expected) yield {
      NormalAttack(text).asIcons() must_== Seq(icon)
      BasicAttack(text).asIcons() must_== Seq(iconAsBasic(icon))
    }
    iconAsBasic(Melee) must_== MeleeBasic
    iconAsBasic(Range) must_== RangeBasic
    iconAsBasic(Close) must_== CloseBasic
    iconAsBasic(Area) must_== AreaBasic

    NormalAttack("melee\narea").asIcons() must_== Seq(Melee, Area)
    BasicAttack("close BlAst\nRaNGed").asIcons() must_== Seq(CloseBasic, RangeBasic)
  }

  private def makeEntry(clazz: String, header: String, body: String) = TextBlock("P", clazz,
    TextSegment.makeItalic(header),
    TextSegment(body))

  private def buildBlock(blocks: TextBlock*) = {
    val bb = new TextBuilder
    for (b <- blocks) bb.append(b)
    bb.getDocument()
  }
}