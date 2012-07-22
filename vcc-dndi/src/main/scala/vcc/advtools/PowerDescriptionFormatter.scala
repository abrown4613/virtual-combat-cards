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

import vcc.infra.text.{TextSegment, TextBlock, TextBuilder, StyledText}
import vcc.advtools.Monster.AttackBonus

object PowerDescriptionFormatter {
  private var debug = false

  def doDebug() {
    debug = true
  }

  def formatAttack(attack: Monster.Attack): StyledText = {
    val builder = new TextBuilder
    try {
      var ls: List[(String, String)] = Nil
      if (!attack.bonuses.isEmpty) {
        ls = "Attack: " -> formatAttackDetails(attack) :: ls
        if (attack.hit.damage.isDefined)
          ls = "Hit: " -> (attack.hit.damage.get + attack.hit.description.map(" " + _).getOrElse(" damage.")) :: ls
      } else {
        ls = "Effect: " -> attack.effect.description.get :: ls
      }
      ls.reverse.foreach(l => builder.append(makeEntry("flavorIndent", l._1, l._2)))
    } catch {
      case _: Throwable =>
        builder.append(TextBlock("P", "", TextSegment.makeBold("NOTFORMATTED")))
    }
    if (debug)
      builder.append(TextBlock("P", "", TextSegment.makeBold("RAW"), TextSegment(attack.toString)))
    builder.getDocument()
  }

  private def formatBonus(bonus: AttackBonus): String = "%+d vs. %s".format(bonus.bonus, bonus.defense)

  private def formatAttackDetails(attack: Monster.Attack): String = {
    val ms = (attack.range ++ attack.targets.map("(%s)".format(_))).mkString("", " ", "; ")
    (if (ms == "; ") "" else ms) + formatBonus(attack.bonuses(0))
  }

  private def makeEntry(clazz: String, header: String, body: String) = TextBlock("P", clazz,
    TextSegment.makeItalic(header),
    TextSegment(body))

}