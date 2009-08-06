/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.controller.actions

import vcc.dnd4e.model.{TrackerCombatant}
import vcc.controller.actions._


abstract class StateChangeAction extends TransactionalAction

case class SetComment(to:Symbol,text:String) extends TransactionalAction {
  def description():String = "Comment of "+to.name+ " has changed"
}

abstract class HealthChangeAction extends TransactionalAction

case class ApplyDamage(to:Symbol,damage:Int) extends HealthChangeAction {
  def description():String = to.name + " takes "+damage + " hitpoints of damage"
}

case class SetTemporaryHP(to:Symbol,temphp:Int) extends HealthChangeAction {
  def description():String = to.name + " recieves "+temphp + " of temporary hitpoints"
}

case class HealDamage(to:Symbol,heal:Int) extends HealthChangeAction {
  def description():String = to.name + " recovers "+heal + " hitpoints"
}

case class FailDeathSave(to:Symbol) extends HealthChangeAction {
  def description():String = to.name + " fails save versus death"
}

case class Undie(to:Symbol) extends HealthChangeAction {
  def description():String = to.name + " is no longer dead"
}


case class QueryCombatantMap[T](func:(TrackerCombatant=>T)) extends QueryAction
