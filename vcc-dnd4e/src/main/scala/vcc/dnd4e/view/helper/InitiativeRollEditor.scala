/*
 *  Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.helper

import javax.swing.text.DefaultFormatter
import java.lang.String
import scala.util.matching.Regex
import java.text.ParseException
import javax.swing.{JTable, JFormattedTextField, DefaultCellEditor}
import java.awt.Component
import vcc.dnd4e.util.{DiceGenerator}

/**
 * Class to hold temporary edits of a set of initiative rolls or instruction to roll
 */
case class InitiativeRoll(rolls: List[Option[Int]]) {
  override def toString: String = rolls.map(r => r match {
    case None => "r"
    case Some(i) => i.toString
  }).mkString("/")

  def isDefined(): Boolean = !rolls.isEmpty

  /**
   * Expand R (or None) are not the result of a D20 and add to initiative bonus
   * @param initBonus Initiative bonus to be added to result
   * @parma dice A dice generator (normally should be DiceBag)
   */
  def resolve(initBonus: Int, dice: DiceGenerator): List[Int] = rolls.map {
    r => r match {
      case None => dice.D(20) + initBonus
      case Some(i) => i + initBonus
    }
  }.toList

  def addRolls(that: InitiativeRoll) = InitiativeRoll(this.rolls ::: that.rolls)
}

object InitiativeRoll {
  val slashTrim: Regex = """\s*\/\s*""".r
  val natural: Regex = """(\d+)""".r

  /**
   * Para a string of slash separated number os upper or lower case r.
   * @throw ParseException If the string is invalid
   */
  def fromString(inString: String): InitiativeRoll = {
    val rl: Seq[Option[Int]] = if (inString.trim == "") {
      Nil
    } else {
      val str = slashTrim.replaceAllIn(inString.trim, "/")
      str.toLowerCase.split("/").map(s => s match {
        case "r" => None
        case natural(i) => Some(i.toInt)
        case s => throw new ParseException("particle '" + s + "' is not one of r or number", inString.indexOf(s))
      })
    }
    InitiativeRoll(rl.toList)
  }

  /**
   * Returns a simple roll.
   */
  val simpleRoll: InitiativeRoll = InitiativeRoll(List(None))
}

/**
 *  This formatter for CellEditor is used to check for InitiativeRoll
 */
class InitiativeRollFormatter extends DefaultFormatter {
  override def valueToString(value: AnyRef): String = {
    value match {
      case ir: InitiativeRoll => ir.toString
      case _ => throw new ParseException("expected InitiativeRoll", 0)
    }
  }

  override def stringToValue(string: String): AnyRef = {
    InitiativeRoll.fromString(string)
  }
}

/**
 * This is a special editor that allow users to input InitiativeRoll in cells. Provides validation and editing.
 */
class InitiativeRollEditor extends DefaultCellEditor(new JFormattedTextField(new InitiativeRollFormatter())) {
  override def getTableCellEditorComponent(table: JTable, value: Any, isSelected: Boolean, row: Int, column: Int): Component = {
    val ftf = super.getTableCellEditorComponent(table, value, isSelected, row, column).asInstanceOf[JFormattedTextField]
    ftf.setValue(value);
    ftf.setToolTipText("Use: blank - to leave combatant out of the initiative order; r - for VCC to roll; number - for actual value; use slash to separate multiple initiative rolls")
    return ftf;
  }

  override def stopCellEditing: Boolean = {
    val ftf = getComponent.asInstanceOf[JFormattedTextField]
    if (ftf.isEditValid()) {
      try {
        ftf.commitEdit();
      } catch {
        case e: ParseException =>
      }
    } else { //text is invalid
      return false; //don't let the editor go away
    }
    super.stopCellEditing()
  }

  override def getCellEditorValue: AnyRef = {
    val ftf = getComponent.asInstanceOf[JFormattedTextField]
    val v = ftf.getValue()
    v
  }
}

