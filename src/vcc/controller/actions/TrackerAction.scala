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
package vcc.controller.actions

/**
 * This is the base class for any action that must be processed within a 
 * transaction.
 */
trait TransactionalAction {
  def description():String
}

/**
 * This is the base class for Query actions.
 */
abstract class QueryAction

case class AddObserver(obs:scala.actors.Actor)
case class StartTransaction(desc:String)
case class EndTransaction(desc:String)
case class ClearTransactionLog()

case class Undo() 
case class Redo()

//case class LogError(msg:String)

