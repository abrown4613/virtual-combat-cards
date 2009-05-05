//$Id$
package vcc.dnd4e.controller.request

import vcc.controller.actions.{TransactionalAction,QueryAction}
import vcc.dnd4e.model.{CombatantTemplate,TrackerCombatant,InitiativeTracker}

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

case class ApplyRest(extended:Boolean) extends EncounterAction {
  def description():String = if(extended) "Extended rest" else "Short rest"
}

case class EndCombat() extends EncounterAction {
  def description():String = "End combat"
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

case class MoveBefore(who:Symbol,before:Symbol) extends InitiativeAction {
  def description():String = who.name + " moving before "+before.name + " in the sequence"
}

case class InternalInitiativeAction(comb:TrackerCombatant,action:InitiativeTracker.actions.Value) extends InitiativeAction {
  def description():String = comb.name + " executed initiative action " + action
}
