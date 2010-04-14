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
package vcc.controller

/**
 * This define the business logic behavior of the Tracker, it allows
 * you to add handlers for processing TransactionalActions.
 * This is designed to be implemented for each game system.
 */
trait TrackerController {

  /**
   * Process a TransactionalAction through all handlers in sequence.
   */
  def dispatch(trans: transaction.Transaction, source: CommandSource, msg: message.TransactionalAction)

  /**
   * Transform ChangeNotification into a single messages.
   * The message will be sent to all registered observer.
   */
  def publish(changes: Seq[transaction.ChangeNotification]): Any
}