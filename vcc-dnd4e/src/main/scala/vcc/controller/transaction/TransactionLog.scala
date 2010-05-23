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
package vcc.controller.transaction

class BadTransaction(msg: String) extends Exception(msg)
class TransactionLogOutOfBounds(msg: String) extends Exception(msg)

/**
 * Container for transactions past and future. The T type is the application
 * specific context information (maybe actions or messaged)
 */
class TransactionLog[T] {
  case class TransactionStore(action: T, trans: Transaction)
  private var pastTrans: List[TransactionStore] = Nil
  private var futureTrans: List[TransactionStore] = Nil

  /**
   * Stores a transaction and associated description into the log. 
   * Transction must be closed
   * @param action Transaction Description
   * @param trans The transaction to be stored
   * @throws BadTransaction if transaction is not commited or repeated
   */
  def store(action: T, trans: Transaction) {
    if (trans == null || action == null)
      throw new BadTransaction("Transcation must be a transaction")
    if (trans.state != Transaction.state.Committed)
      throw new BadTransaction("Transcation is not commited")
    if (trans.isEmpty)
      throw new BadTransaction("Transcation is empty")
    if (pastTrans.exists(x => {x.trans eq trans}))
      throw new BadTransaction("Transcation already in log")

    pastTrans = TransactionStore(action, trans) :: pastTrans
    futureTrans = Nil //Future must be cleaned
  }

  def length = pastTrans.length

  /**
   * Retuns the list of actions that can be rolled back
   */
  def pastActions(): List[T] = pastTrans.map(x => x.action)

  /**
   * Return the list of future action, that is actions that can be rolled forward
   */
  def futureActions(): List[T] = futureTrans.map(x => x.action)

  /**
   * Undo first past transaction
   */
  @throws(classOf[TransactionLogOutOfBounds])
  def rollback(publisher: TransactionChangePublisher) {
    if (pastTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollback to")
    var entry = pastTrans.head
    entry.trans.undo(publisher)
    futureTrans = entry :: futureTrans
    pastTrans = pastTrans.tail
  }

  /**
   * Redo first future transaction
   */
  @throws(classOf[TransactionLogOutOfBounds])
  def rollforward(publisher: TransactionChangePublisher) {
    if (futureTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollforward to")
    var entry = futureTrans.head
    entry.trans.redo(publisher)
    futureTrans = futureTrans.tail
    pastTrans = entry :: pastTrans
  }

  /**
   * Clear log both past and future
   */
  def clear() {
    pastTrans = Nil
    futureTrans = Nil
  }
}
