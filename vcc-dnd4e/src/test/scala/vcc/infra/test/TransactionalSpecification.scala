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

import vcc.controller.transaction.{ChangeNotification, Transaction}
import org.specs.Specification

trait TransactionalSpecification {
  self: Specification =>

  class AfterCommit(action: Transaction => Any) {
    def afterCommit(after: Seq[ChangeNotification] => Any) = {
      "commit" in {
        val trans = new Transaction()
        action(trans)
        val cLogger = new TransactionChangeLogger()
        trans.commit(cLogger)
        after(cLogger.changes)
      }
      new AfterUndo(action, after)
    }
  }

  class AfterUndo(action: Transaction => Any, commitExample: Seq[ChangeNotification] => Any) {
    def afterUndo(after: Seq[ChangeNotification] => Any) = {
      "undo" in {
        val trans = new Transaction()
        action(trans)
        val cLogger = new TransactionChangeLogger()
        trans.commit(cLogger)
        trans.undo(cLogger)
        after(cLogger.changes)
      }
      new AfterRedo(action, commitExample)
    }
  }

  class AfterRedo(action: Transaction => Any, commitExample: Seq[ChangeNotification] => Any) {
    def afterRedo(after: Seq[ChangeNotification] => Any) = {
      "redo" in {
        val trans = new Transaction()
        action(trans)
        val cLogger = new TransactionChangeLogger()
        trans.commit(cLogger)
        trans.undo(cLogger)
        trans.redo(cLogger)
        after(cLogger.changes)
      }
    }

    def afterRedoAsInCommit() = {
      "redo as in commit" in {
        val trans = new Transaction()
        action(trans)
        val cLogger = new TransactionChangeLogger()
        trans.commit(cLogger)
        trans.undo(cLogger)
        trans.redo(cLogger)
        commitExample(cLogger.changes)
      }
    }
  }

  /**
   * Define an action that must be executed in a transactional context. 
   */
  def withTransaction(action: Transaction => Any) = {
    new AfterCommit(action)
  }

  /**
   * Run and Commit an action
   */
  def runAndCommit(action: Transaction => Any) = {
    val trans = new Transaction()
    val cLogger = new TransactionChangeLogger()
    action(trans)
    trans.commit(cLogger)
  }
}
