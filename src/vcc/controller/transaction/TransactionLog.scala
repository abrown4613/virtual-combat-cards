//$Id$
package vcc.controller.transaction

trait TransactionDescription {
  def description:String
}

class BadTransaction(msg:String) extends Exception(msg)
class TransactionLogOutOfBounds(msg:String) extends Exception(msg)

/**
 * Container for transactions past and future
 */
class TransactionLog[T<:TransactionDescription] {
  
  case class TransactionStore(desc:T, trans:Transaction)
  private var pastTrans:List[TransactionStore]=Nil
  private var futureTrans:List[TransactionStore]=Nil
  
  /**
   * Stores a transaction and associated description into the log. 
   * Transction must be closed
   * @param desc Transaction Description
   * @param trans The transaction to be stored
   * @throws BadTransaction if transaction is not commited or repeated
   */
  def store(desc:T, trans:Transaction) {
    if(trans==null || desc==null)
      throw new BadTransaction("Transcation must be a transaction")
    if(trans.state != Transaction.state.Committed)
      throw new BadTransaction("Transcation is not commited")
    if(trans.isEmpty )
      throw new BadTransaction("Transcation is empty")
    if(pastTrans.exists(x=>{x.trans eq trans}))
      throw new BadTransaction("Transcation already in log")

    pastTrans=TransactionStore(desc,trans)::pastTrans
  }

  def length = pastTrans.length
  
  def previousTransctionDescription:String = if(pastTrans.isEmpty) null else pastTrans.head.desc.description
  
  def nextTransactionDescription:String = if(futureTrans.isEmpty) null else futureTrans.head.desc.description
  
  def rollback(publisher: TransactionChangePublisher) {
    if(pastTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollback to")
    var entry=pastTrans.head
    entry.trans.undo(publisher)
    futureTrans=entry::futureTrans
    pastTrans=pastTrans.tail
  }

  def rollforward(publisher: TransactionChangePublisher) {
    if(futureTrans.isEmpty)
      throw new TransactionLogOutOfBounds("Nothing in the past to rollforward to")
    var entry=futureTrans.head
    entry.trans.redo(publisher)
    futureTrans=futureTrans.tail
    pastTrans=entry::pastTrans
    
  }
}
