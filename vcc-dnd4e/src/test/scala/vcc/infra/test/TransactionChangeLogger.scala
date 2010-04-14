/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.test

import vcc.controller.transaction.{ChangeNotification, TransactionChangePublisher}

/**
 * This is a helper class to collect all the Transaction ChangeNotification into a log.
 */
class TransactionChangeLogger extends TransactionChangePublisher {
  private var _cLog: Seq[ChangeNotification] = Nil

  def publishChange(changes: Seq[ChangeNotification]) {
    _cLog = changes
  }

  /**
   * ChangeNotification captured in last publishChange.
   * @return Sequence of ChangeNotification 
   */
  def changes = _cLog

}
