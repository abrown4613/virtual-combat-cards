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
package test

import junit.framework.TestCase
import vcc.controller.transaction._

class TransactionLogTest extends TestCase {

  case class MyTrans(msg:String)
  
  def testEmptyLog() {
    val tlog= new TransactionLog[MyTrans]()
    
    assert(tlog.futureActions==Nil)
    assert(tlog.pastActions==Nil)
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
    val tlog= new TransactionLog[MyTrans]()
    try {
      tlog.store(null,null)
      assert(false,"Must not save null transaction")
    } catch {
      case s:BadTransaction => assert(true)
    }
    
  }
  
  def testStoreOpenTransaction() {
    val tlog= new TransactionLog[MyTrans]()
    val trans= new Transaction()
    
    try {
      tlog.store(MyTrans("First"),trans)
      assert(false,"Must not save open transaction")
    } catch {
      case s:BadTransaction => assert(true)
    }
  }

  def testStoreEmptyTransaction() {
    val tlog= new TransactionLog[MyTrans]()
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
    val tlog= new TransactionLog[MyTrans]()
    implicit var trans= new Transaction()
    val v= new Undoable(10,null)

    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)
    assert(tlog.length==1)
    assert(tlog.pastActions==List(MyTrans("First")))
    assert(tlog.futureActions==Nil)
  }

  /**
   * Test saving a transaction and making sure that notification go out on save, rollback and rollforward.
   */
  def testStoreAndRollbackThenRollFoward() {
    val tlog= new TransactionLog[MyTrans]()
    implicit var trans= new Transaction()
    val v= new Undoable[Int](10,(x=>Beep('nu1,x.value)))
    val bp=new BeepOut
    
    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)
    assert(tlog.length==1)
    assert(tlog.pastActions==List(MyTrans("First")))
    assert(tlog.futureActions==Nil)
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,10)))
    assert(tlog.pastActions==Nil)
    assert(tlog.futureActions==List(MyTrans("First")))
    
    bp.changes=Nil
    tlog.rollforward(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.pastActions==List(MyTrans("First")))
    assert(tlog.futureActions==Nil)
    
  }
  
  /**
   * This is the last test make two transactions, rollback one, and start save a 
   * replacement transaction, this should make the future transcation change. 
   */
  def testStoreStoreAndRollbackThenStore() {
    val tlog= new TransactionLog[MyTrans]()
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
    assert(tlog.pastActions==List(MyTrans("Second"),MyTrans("First")))
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.futureActions==List(MyTrans("Second")))
    assert(tlog.pastActions==List(MyTrans("First")))
    bp.changes=Nil
    
    trans=new Transaction()
    v.value=22
    trans.commit(null)
    tlog.store(MyTrans("Another path"),trans)
    assert(tlog.pastActions==List(MyTrans("Another path"),MyTrans("First")))
    assert(tlog.futureActions==Nil)
    
    tlog.rollback(bp)
    assert(bp.changes.length==1)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(tlog.futureActions==List(MyTrans("Another path")),"Found "+tlog.futureActions)
    assert(tlog.pastActions==List(MyTrans("First")))
  } 
  
  def testStoreSameTransactionTwice {
    val tlog= new TransactionLog[MyTrans]()
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

  /**
   * Empty the log.
   */
  def testClearLog() {
    val tlog= new TransactionLog[MyTrans]()
    implicit var trans= new Transaction()
    val v= new Undoable[Int](10,null)

    v.value=20
    trans.commit(null)
    tlog.store(MyTrans("First"),trans)

    trans=new Transaction()
    v.value=30
    trans.commit(null)
    tlog.store(MyTrans("Second"),trans)
    assert(tlog.pastActions==List(MyTrans("Second"),MyTrans("First")))
    
    tlog.rollback(null)
    assert(tlog.futureActions==List(MyTrans("Second")))
    assert(tlog.pastActions==List(MyTrans("First")))

    tlog.clear()
    assert(tlog.length==0)
    assert(tlog.futureActions==Nil)
    assert(tlog.pastActions==Nil)
    
  }
}
