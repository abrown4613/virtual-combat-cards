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
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.domain.tracker.common.CombatMetaDataChange
import vcc.controller.transaction._

/**
 * Contains data about the combat state not related to InitiativeOrder or to Combatant and their Roster.
 * @param initialText The initial combat comment string
 */
class CombatMetaData(initialText: String) extends ChangeNotifier {
  def this() = this ("")

  private val _comment = new Undoable[String](initialText, x => ChangeNotificationPromise(this))
  private val _inCombat = new Undoable[Boolean](false, x => ChangeNotificationPromise(this))

  def createNotification(): ChangeNotification = CombatMetaDataChange(_inCombat.value, _comment.value)

  def startCombat()(implicit trans: Transaction) {_inCombat.value = true}

  def endCombat()(implicit trans: Transaction) {_inCombat.value = false}

  def inCombat = _inCombat.value

  def comment_=(text: String)(implicit trans: Transaction) {_comment.value = text}

  def comment: String = _comment.value
}