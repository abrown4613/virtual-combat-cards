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

import vcc.util.swing.TableModelRowProjection
import vcc.util.DiceBag
import vcc.dnd4e.domain.tracker.common.CombatantID
import vcc.dnd4e.view.helper.InitiativeRoll

//NOTE: These classes should be internal to the Dialog 

class InitiativeDialogEntry(val id: CombatantID, val name: String, var init: Int, var roll: InitiativeRoll) {
  override def toString(): String = "IDEntry(" + id + "," + name + "," + init + "," + roll + ")"
}

object InitiativeDialogEntryProjection extends TableModelRowProjection[InitiativeDialogEntry] {
  val columns = List[(String, java.lang.Class[_])](
    ("ID", classOf[String]),
    ("Name", classOf[String]),
    ("Bonus", classOf[Integer]),
    ("Roll", classOf[InitiativeRoll]))

  def apply(col: Int, entry: InitiativeDialogEntry): java.lang.Object = {
    col match {
      case 0 => entry.id.id
      case 1 => entry.name
      case 2 => int2Integer(entry.init)
      case 3 => entry.roll
    }
  }

  val setter: PartialFunction[(Int, InitiativeDialogEntry, Any), Unit] = {
    case (2, entry, v) => entry.init = v.asInstanceOf[Int]
    case (3, entry, v) => entry.roll = v.asInstanceOf[InitiativeRoll]
  }
}
