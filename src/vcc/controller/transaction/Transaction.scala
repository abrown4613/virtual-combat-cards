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

/**
 * When transactions are commit, undone, or redo implementations of this
 * trait can be used to notify observers that there has been changes in
 * the underlying object
 */
trait TransactionChangePublisher {
  
  /**
   * Notify observers of some transaction operations (commit, undo or redo)
   * @param changes : Changes that happened when executing the transcation operation
   */
  def publishChange(changes:Seq[ChangeNotification])
}

/**
 * Occurs when trying to add mementos to a closed transaction, on when trying to 
 * cancel a closed transaction 
 */
class TransactionClosedException() extends Exception

/**
 * Occurs when an Undo,Commit, Cancel or Redo is applied to a transaction in a state that does
 * not allow that operations. e.g. Cancel a cancelled operation, or redo a committed transact
 */
class InvalidTransactionOperationException(from:Transaction.state.Value,to:Transaction.state.Value) extends Exception {
  override def getMessage():String = "Illegal Transaction operation: Cant go from "+from +" to "+to
}

object Transaction {
  object state extends Enumeration {
    val Active = Value("Active")
    val Committed = Value("Committed")
    val Cancelled = Value("Cancelled")
    val Undone = Value("Undone")
  }
}

/**
 * This represent a single transaction, to be supplied to Undoable[T] so that 
 * they can store mementos. 
 * Transaction can be Cancelled, Committed, Undone and Redone. Any operation that
 * changes Undoable objects can have changes published via the TransactionChangePublisher
 * trait.
 */
class Transaction {
  import Transaction.state._
  
  private var _state = Active 
  
  private var _parts:List[UndoMemento[_]]=Nil
  
  /**
   * Add a single menento to the transactions. 
   * Current implementation may store many entry for the same Undoable
   */
  def addMemento(x:UndoMemento[_]) {
    //TODO: Remove double entry of a transaction
    if(_state != Active)
      throw new TransactionClosedException
    if(!_parts.exists(u=>(u.obj eq x.obj)))
      _parts=x::_parts
  }
  
  /**
   * Swap store mementos for current values (as mementos)
   */
  protected def swapMementos() {
    // Save current values for a redo and restore values
    var current=_parts map (x=>x.redoMemento)
    for(x<-_parts) x.undo
    _parts=current
  }
  
  /**
   * Undo all changes, and then close the transactions. 
   */
  def cancel() {
    if(_state != Active)
      throw new InvalidTransactionOperationException(_state,Cancelled)
    _state=Cancelled
    for(um<-_parts) {
      um.undo
    }
  }
  
  /**
   * Redo a transactions, this will reapply the changes, transaction must be
   * in Undone state.
   */
  def redo(tcp:TransactionChangePublisher) {
    if(_state!=Undone)
      throw new InvalidTransactionOperationException(_state,Committed)
    swapMementos
    _state=Committed
    if(tcp!=null) publishChange(tcp)
  }
  
  /**
   * Undo a transaction, this will restored values prior to the transactions. Transaction
   * must be in Committed state.
   * @param tcp Object that will publish changes
   */
  def undo(tcp:TransactionChangePublisher) {
    if(_state!=Committed)
      throw new InvalidTransactionOperationException(_state,Undone)
    swapMementos
    _state=Undone
    if(tcp!=null) publishChange(tcp)
  }
  
  /**
   * Commit a transaction, this will make the transaction read only, can only be done from
   * an Active transactions.
   * @param tcp Object that will publish changes
   */
  def commit(tcp:TransactionChangePublisher) {
    if(_state != Active)
      throw new InvalidTransactionOperationException(_state,Committed)
    _state= Committed
    if(tcp!=null) publishChange(tcp)
  }
  
  /**
   * Publish changes to observers.
   * @param tcp Object that will publish changes
   */
  protected def publishChange(tcp:TransactionChangePublisher) {
    val changes=scala.collection.mutable.Set.empty[ChangeNotification]
    for(x <- _parts) {
      val change=x.changeNotification 
      if(change.isDefined) changes+=change.get
    }
    
    // Expand promises
    var exp_changes=changes.map({
      case ChangeNotificationPromise(cn) => cn.createNotification 
      case x => x 
    }).toSeq
    tcp.publishChange(exp_changes)
  }
  
  /**
   * Get the current transaction state.
   */
  def state=_state
  
  /**
   * Return if the transaction is empty or not. Active transactions are not empty, no 
   * matter how many mementos they contain.
   */
  def isEmpty = ((_state!=Active) && _parts.isEmpty)
}
