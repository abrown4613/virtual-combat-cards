//$Id$
package vcc.dnd4e.controller.actions

import vcc.dnd4e.model.{CombatantTemplate,TrackerCombatant}
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
