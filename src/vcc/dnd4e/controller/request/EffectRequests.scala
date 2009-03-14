//$Id$
package vcc.dnd4e.controller.request

import vcc.controller.actions.{TransactionalAction,QueryAction} 
import vcc.dnd4e.model.{Effect,Condition}

case class AddEffect(to:Symbol,effect:Effect) extends TransactionalAction {
  def description = "Add effect "+effect+ " to "+to
}

case class CancelEffect(to:Symbol,pos:Int) extends TransactionalAction {
  def description = "Cancel effect number "+pos + " of "+to
}

case class SustainEffect(to:Symbol,pos:Int) extends TransactionalAction {
  def description = "Sustain effect number "+pos + " of "+to
}

/**
 * Update and effect of a symbol
 * @param to The combatant to update
 * @param pos Effect position
 * @param cond The new condition
 */
case class UpdateEffect(to:Symbol,pos:Int,cond:Condition) extends TransactionalAction {
  def description = "Update effect number "+pos + " of "+to+ " to value "+cond
}
