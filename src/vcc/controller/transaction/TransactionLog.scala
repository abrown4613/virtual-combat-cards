//$Id$
package vcc.controller.transaction

class BadTransaction(msg:String) extends Exception(msg)
class TransactionLogOutOfBounds(msg:String) extends Exception(msg)

/**
 * Container for transactions past and future. The T type is the application
 * specific context information (maybe actions or messaged)
 */
class TransactionLog[T] {
  
  case class TransactionStore(action:T, trans:Transaction)
  private var pastTrans:List[TransactionStore]=Nil
  private var futureTrans:List[TransactionStore]=Nil
  
  /**
   * Stores a transaction and associated description into the log. 
   * Transction must be closed
   * @param action Transaction Description
   * @param trans The transaction to be stored
   * @throws BadTransaction if transaction is not commited or repeated
   */
  def store(action:T, trans:Transaction) {
    if(trans==null || action==null)
      throw new BadTransaction("Transcation must be a transaction")
    if(trans.state != Transaction.state.Committed)
      throw new BadTransaction("Transcation is not commited")
    if(trans.isEmpty )
      throw new BadTransaction("Transcation is empty")
    if(pastTrans.exists(x=>{x.trans eq trans}))
      throw new BadTransaction("Transcation already in log")

    pastTrans=TransactionStore(action,trans)::pastTrans
    futureTrans=Nil //Future must be cleaned
  }

  def length = pastTrans.length
  
  /**
   * Retuns the list of actions that can be rolled back
   */
  def pastActions():List[T] = pastTrans.map(x=>x.action)
  
  /**
   * Return the list of future action, that is actions that can be rolled forward
   */
  def futureActions():List[T] = futureTrans.map(x=>x.action)
  
  /**
   * Undo first past transaction
   */
  def rollback(publisher: TransactionChangePublisher) {
    if(pastTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollback to")
    var entry=pastTrans.head
    entry.trans.undo(publisher)
    futureTrans=entry::futureTrans
    pastTrans=pastTrans.tail
  }

  /**
   * Redo first future transaction
   */
  def rollforward(publisher: TransactionChangePublisher) {
    if(futureTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollforward to")
    var entry=futureTrans.head
    entry.trans.redo(publisher)
    futureTrans=futureTrans.tail
    pastTrans=entry::pastTrans
  }
  
  /**
   * Clear log both past and future
   */
  def clear() {
    pastTrans=Nil
    futureTrans=Nil
  }
}
