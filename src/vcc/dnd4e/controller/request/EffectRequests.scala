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
package vcc.dnd4e.controller.request

import vcc.controller.message.{TransactionalAction} 
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
