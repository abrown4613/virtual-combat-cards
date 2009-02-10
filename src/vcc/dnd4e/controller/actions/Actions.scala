//$Id$
package vcc.dnd4e.controller.actions

import vcc.dnd4e.model.{CombatantTemplate,TrackerCombatant}
import vcc.controller.actions._

// Builing the combat grid

abstract class EncounterAction extends TransactionalAction

case class AddCombatant(template:CombatantTemplate) extends EncounterAction {
  def description():String = "Add combatant"
}
case class Enumerate() extends QueryAction

case class ClearCombatants(all:Boolean) extends EncounterAction {
  def description():String = "Clear "+(if(all) "all combatants" else "monsters")
}

case class StartCombat(order:Seq[Symbol]) extends EncounterAction {
  def description():String = "Start combat, with order: "+order
}

case class EndCombat() extends EncounterAction {
  def description():String = "End combat"
}

case class ApplyRest(extended:Boolean) extends EncounterAction {
  def description():String = if(extended) "Extended rest" else "Short rest"
}


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


/**
 * Master class for all initiave based actions
 */
abstract case class InitiativeAction() extends TransactionalAction

case class StartRound(who:Symbol) extends InitiativeAction {
  def description():String = "Start of "+ who.name + "'s round"
}
case class EndRound(who:Symbol) extends InitiativeAction {
  def description():String = "End of "+ who.name + "'s round"
}
case class Ready(who:Symbol) extends InitiativeAction {
  def description():String = "Ready action by "+ who.name
}
case class Delay(who:Symbol) extends InitiativeAction {
  def description():String = "Delay action by "+ who.name
}
case class MoveUp(who:Symbol) extends InitiativeAction {
  def description():String =  who.name + " has moved up round"
}
case class ExecuteReady(who:Symbol) extends InitiativeAction {
  def description():String = who.name + " executed a ready action"
}

case class QueryCombatantMap[T](func:(TrackerCombatant=>T)) extends QueryAction
