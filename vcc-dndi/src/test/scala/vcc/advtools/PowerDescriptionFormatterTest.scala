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
import vcc.advtools.Monster._
import vcc.infra.text._
import vcc.advtools.MonsterReaderHelper.makeUtilityAttack
import scala.Some

class PowerDescriptionFormatterTest extends SpecificationWithJUnit {

  private val tagClass = "flavorIndent"

  "format utility" in {
    val block = buildBlock(
      makeEntry(tagClass, "Effect: ", "some effect"))

    PowerDescriptionFormatter.formatAttack(makeUtilityAttack("some damage", "some effect")) must_== block
  }

  "format attack simple" in {
    val attack = Attack(List(AttackBonus("AC", 11)), Some("1d8+4"), None)

    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "+11 vs. AC"),
      makeEntry(tagClass, "Hit: ", "1d8+4 damage."))

    PowerDescriptionFormatter.formatAttack(attack) must_== block
  }

  "format attack with effect and range" in {
    val attack = Attack(List(AttackBonus("AC", 11)), Some("Melee 3"), None, Some("1d8+4"), Some("slowed EOT"))

    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "Melee 3; +11 vs. AC"),
      makeEntry(tagClass, "Hit: ", "1d8+4 slowed EOT"))

    PowerDescriptionFormatter.formatAttack(attack) must_== block
  }

  "format attack with range and targets" in {
    val attack = Attack(List(AttackBonus("Will", 13)), Some("Ranged 5"), Some("one creature"), Some("2d12+8"), Some("damage and the target is dominated (save ends)."))

    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "Ranged 5 (one creature); +13 vs. Will"),
      makeEntry(tagClass, "Hit: ", "2d12+8 damage and the target is dominated (save ends)."))

    PowerDescriptionFormatter.formatAttack(attack) must_== block
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