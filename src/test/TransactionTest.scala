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

import junit.framework._

import vcc.controller.transaction._

case class Beep[T](s:Symbol,v:T) extends ChangeNotification

class BeepOut extends TransactionChangePublisher {
  var changes:Seq[ChangeNotification]=Nil
  def publishChange(c:Seq[ChangeNotification]) {
    changes=c
  }
}

class TransactionTest extends TestCase {
  
  /**
   * Story:
   * Do several transactions, change one entry once, change another twice
   * receive notifications from only one of each undoable, more important only called once
   */
  def testRegularTransaction {
    var nu1=0
    var touched=Set.empty[Undoable[_]]
    val u1=new Undoable[Int](10,(x=>{
      if(touched.contains(x))
        assert(false,"Should not call notification function twice in same transaction")
      else
        touched=touched +x 
      Beep('nu1,x.value)
    }))
    val u2=new Undoable[String]("test",(x=>Beep('nu2,x.value)))
    val u3=new Undoable[Int](30,null)
    val bp=new BeepOut
    implicit val trans=new Transaction
    
    u1.value=15
    assert(u1.value==15)
    u1.value=20
    u2.value="test2"
    assert(u1.value==20)
    assert(u2.value=="test2")
    
    trans.commit(bp)
    assert(!trans.isEmpty)
    assert(bp.changes.size==2)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(bp.changes.contains(Beep('nu2,"test2")))

    
    assert(trans.state == Transaction.state.Committed)
    // Cant writ to commited transaction
    try {
      u2.value="test3"
      assert(false,"Must not write to commited transaction")
    } catch {
      case e:TransactionClosedException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    // Cant cancel commited transaction
    try {
      trans.cancel
      assert(false,"Cant cancel commited transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    0
  }

  /**
   * Story: Cancel transaction after updating
   */
  def testCancelTransaction {
    var nu1=0
    val u1=new Undoable[Int](10,(x=>Beep('nu1,x.value)))
    val u2=new Undoable[String]("test",(x=>Beep('nu2,x.value)))
    val u3=new Undoable[Int](30,null)
    val bp=new BeepOut
    implicit val trans=new Transaction
    
    u1.value=15
    assert(u1.value==15)
    u1.value=20
    u2.value="test2"

    assert(u1.value==20)
    assert(u2.value=="test2")

    trans.cancel()
    
    assert(u1.value==10)
    assert(u2.value=="test")
    
    assert(trans.state == Transaction.state.Cancelled)
    // Should not be able to write a cancelled transactions
    try {
      u2.value="test3"
      assert(false,"Must not write to commited transaction")
    } catch {
      case e:TransactionClosedException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }

    // Should not be able to cancel a cancelled transactions
    try {
      trans.cancel
      assert(false,"Must cancel cancelled transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    // Cant cancel commited transaction
    try {
      trans.commit(null)
      assert(false,"Cant commit cancelled transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }

    try {
      trans.undo(null)
      assert(false,"Cant undo cancelled transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    
    try {
      trans.redo(null)
      assert(false,"Cant undo cancelled transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    
    0
  }

  /**
   * Story: Test undo and then redoing a transactions then undo and redo
   */
  def testUndoThenRedo {
    val u1=new Undoable[Int](10,(x=>Beep('nu1,x.value)))
    val u2=new Undoable[Int](10,(x=>Beep('nu2,x.value)))
    val bp=new BeepOut
    implicit val trans=new Transaction
    
    u1.value=20
    u2.value=30
    assert(u1.value==20)
    assert(u2.value==30)

    try {
      trans.undo(bp)
      assert(false,"Must not undo open transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }

    try {
      trans.redo(bp)
      assert(false,"Must not redo open transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }

    trans.commit(null)
    assert(trans.state == Transaction.state.Committed)
    try {
      u2.value=40
      assert(false,"Must not write to commited transaction")
    } catch {
      case e:TransactionClosedException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    
    trans.undo(bp)
    // Cant undo undone commited transaction
    try {
      trans.undo(null)
      assert(false,"Cant undo undone transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }
    
    assert(trans.state==Transaction.state.Undone)
    assert(u1.value==10)
    assert(u2.value==10)

    assert(bp.changes.size==2)
    assert(bp.changes.contains(Beep('nu1,10)))
    assert(bp.changes.contains(Beep('nu2,10)))

    trans.redo(bp)
    try {
      trans.redo(bp)
      assert(false,"Cant redo redone transaction")
    } catch {
      case e:InvalidTransactionOperationException => assert(true)
      case s => assert(false,"Unexpected transaction"+s)
    }

    assert(trans.state==Transaction.state.Committed)
    
    assert(u1.value==20)
    assert(u2.value==30)

    assert(bp.changes.size==2)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(bp.changes.contains(Beep('nu2,30)))
    
    // Second round Undo and redo
    trans.undo(bp)
    assert(trans.state==Transaction.state.Undone)
    assert(u1.value==10)
    assert(u2.value==10)

    assert(bp.changes.size==2)
    assert(bp.changes.contains(Beep('nu1,10)))
    assert(bp.changes.contains(Beep('nu2,10)))

    trans.redo(bp)
    assert(trans.state==Transaction.state.Committed)
    
    assert(u1.value==20)
    assert(u2.value==30)

    assert(bp.changes.size==2)
    assert(bp.changes.contains(Beep('nu1,20)))
    assert(bp.changes.contains(Beep('nu2,30)))

    0
  }

  /**
   * Some object may want to notify changes to the entire object, not to parts, this is
   * to test the changes of parts of an object that have to become one change
   */
  def testNotificationPromise {
    var nu1=0
    var cn =new ChangeNotifier {
      val u1=new Undoable[Int](10,(x=>ChangeNotificationPromise(this)))
      val u2=new Undoable[Int](10,(x=>ChangeNotificationPromise(this)))
      def createNotification()=Beep('all,(u1.value,u2.value))
    }

    val bp=new BeepOut
    implicit val trans=new Transaction
    
    cn.u1.value=20
    cn.u2.value=30
    trans.commit(bp)
    assert(trans.state == Transaction.state.Committed)
    
    assert(bp.changes.size==1)
    assert(bp.changes.contains(Beep('all,(20,30))))

    trans.undo(bp)
    assert(bp.changes.size==1)
    assert(bp.changes.contains(Beep('all,(10,10))))
  }
  
  /**
   * Story: If nothing happens in a transaction then it must be empty
   */
  def testEmptyTransaction {
    val trans=new Transaction
    
    assert(!trans.isEmpty)
    trans.commit(null)
    assert(trans.isEmpty)
  }
}
