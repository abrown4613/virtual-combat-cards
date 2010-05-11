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
package vcc.dnd4e.view.dialog

import vcc.dnd4e.domain.tracker.common.CombatantID
import vcc.dnd4e.view.helper.InitiativeRoll
import vcc.util.swing.{ProjectionTableLabelFormatter, TableModelRowProjection}
import javax.swing.{SwingConstants, JLabel}
import java.awt.Color


class InitiativeDialogEntry(val ids: Set[CombatantID], val name: String, var init: Int, var roll: InitiativeRoll, var skip: Boolean) {
  override def toString(): String = "IDEntry(" + ids + "," + name + "," + init + "," + roll + "," + (if (skip) "skip" else "roll") + ")"

  def isSimilar(that: InitiativeDialogEntry): Boolean = (this.name == that.name) &&
          (this.init == that.init) && (this.roll == that.roll) && (this.skip == that.skip)

  def merge(that: InitiativeDialogEntry): InitiativeDialogEntry =
    new InitiativeDialogEntry(this.ids ++ that.ids, this.name, this.init, this.roll, this.skip)

  def addRolls(that: InitiativeRoll) =
    new InitiativeDialogEntry(this.ids, this.name, this.init, this.roll.addRolls(that), this.skip)
}

object InitiativeDialogEntryProjection extends TableModelRowProjection[InitiativeDialogEntry] {
  val columns = List[(String, java.lang.Class[_])](
    ("ID", classOf[String]),
    ("Name", classOf[String]),
    ("Bonus", classOf[Integer]),
    ("Roll", classOf[InitiativeRoll]),
    ("Skip", classOf[Boolean]))

  def apply(col: Int, entry: InitiativeDialogEntry): java.lang.Object = {
    col match {
      case 0 => entry.ids.elements.map(_.id).mkString(", ")
      case 1 => entry.name
      case 2 => int2Integer(entry.init)
      case 3 => entry.roll
      case 4 => boolean2Boolean(entry.skip)
    }
  }

  val setter: PartialFunction[(Int, InitiativeDialogEntry, Any), Unit] = {
    case (2, entry, v) => entry.init = v.asInstanceOf[Int]
    case (3, entry, v) => entry.roll = v.asInstanceOf[InitiativeRoll]
    case (4, entry, v) => entry.skip = v.asInstanceOf[Boolean]
  }
}

class InitiativeDialogEntryFormatter extends ProjectionTableLabelFormatter[InitiativeDialogEntry] {
  final private val grayed = (Color.LIGHT_GRAY, Color.BLACK)
  final private val skipped = (Color.LIGHT_GRAY, Color.GRAY)
  final private val normal = (Color.WHITE, Color.BLACK)

  def render(label: JLabel, column: Int, isSelected: Boolean, entry: InitiativeDialogEntry) {
    label.setHorizontalAlignment(if (column == 1) SwingConstants.LEFT else SwingConstants.CENTER)
    if (isSelected) setColorPair(label, getColorPair(label))
    else if (entry.skip) setColorPair(label, skipped)
    else if (!entry.roll.isDefined) setColorPair(label, grayed)
    else setColorPair(label, normal)
  }
}