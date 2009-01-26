//$Id$
package test

import junit.framework.TestCase
import vcc.controller.transaction._

class TransactionLogTest extends TestCase {

  case class MyTrans(msg:String) extends TransactionDescription {
    def description=msg
  }
  
  def testEmptyLog() {
    val tlog= new TransactionLog()
    
    assert(tlog.previousTransctionDescription==null)
    assert(tlog.nextTransactionDescription==null)
    assert(true)
    
    try {
      tlog.rollback(null)
      assert(false,"Cant rollback on empty log")
    } catch {
      case s:TransactionLogOutOfBounds => assert(true)
    }
    
    try {
      tlog.rollforward(null)
      assert(false,"Cant rollforward on empty log")
    } catch {
      case s:TransactionLogOutOfBounds => assert(true)
    }
  }
  
  def testStoreNull() {
    val tlog= new TransactionLog()
    try {
      tlog.store(null,null)
      assert(false,"Must not save null transaction")
    } catch {
      case s:BadTransaction => assert(true)
    }
    
  }
  
  def testStoreOpenTransaction() {
    val tlog= new TransactionLog()
    val trans= new Transaction()
    
    try {
      tlog.store(MyTrans("First"),trans)
      assert(false,"Must not save open transaction")
    } catch {
      case s:BadTransaction => assert(true)
    }
  }

  def testStoreEmptyTransaction() {
    val tlog= new TransactionLog()
    val trans= new Transaction()
  
    trans.commit(null)
    try {
      tlog.store(MyTrans("First"),trans)
      assert(false,"Must store not empty transaction")
    } catch {
      case s:BadTransaction => assert(true)
    }
  }

  def testStore() {
    val tlog= new TransactionLog()
    implicit var trans= new Transaction()
    val v= new Undoable(10,null)

    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)
    assert(tlog.length==1)
    assert(tlog.previousTransctionDescription=="First")
    assert(tlog.nextTransactionDescription==null)
  }

  /**
   * Test saving a transaction and making sure that notification go out on save, rollback and rollforward.
   */
  def testStoreAndRollbackThenRollFoward() {
    val tlog= new TransactionLog()
    implicit var trans= new Transaction()
    val v= new Undoable[Int](10,(x=>Beep('nu1,x.value)))
    val bp=new BeepOut
    
    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)
    assert(tlog.length==1)
    assert(tlog.previousTransctionDescription=="First")
    assert(tlog.nextTransactionDescription==null)
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,10)))
    assert(tlog.nextTransactionDescription=="First")
    assert(tlog.previousTransctionDescription==null)
    
    bp.changes=Nil
    tlog.rollforward(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.nextTransactionDescription==null)
    assert(tlog.previousTransctionDescription=="First")
    
  }
  
  /**
   * This is the last test make two transactions, rollback one, and start save a 
   * replacement transaction, this should make the future transcation change. 
   */
  def testStoreStoreAndRollbackThenStore() {
    val tlog= new TransactionLog()
    implicit var trans= new Transaction()
    val v= new Undoable[Int](10,(x=>Beep('nu1,x.value)))
    val bp=new BeepOut

    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)

    trans=new Transaction()
    v.value=30
    trans.commit(null)
    tlog.store(MyTrans("Second"),trans)
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.nextTransactionDescription=="Second")
    assert(tlog.previousTransctionDescription=="First")
    bp.changes=Nil
    
    trans=new Transaction()
    v.value=22
    trans.commit(null)
    tlog.store(MyTrans("Another path"),trans)
    assert(tlog.previousTransctionDescription=="Another path")
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.nextTransactionDescription=="Another path")
    assert(tlog.previousTransctionDescription=="First")
    
  } 
  
  def testStoreSameTransactionTwice {
    val tlog= new TransactionLog()
    implicit var trans= new Transaction()
    val v= new Undoable[Int](10,(x=>Beep('nu2,x.value)))

    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)
    try {
      tlog.store(MyTrans("Repeat First"),trans)
      assert(false,"Cant save transaction twice") 
    } catch {
      case s:BadTransaction => assert(true)
    }
  }
}
