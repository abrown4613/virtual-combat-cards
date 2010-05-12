/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.controller.transaction.{ChangeNotification, Transaction}
import vcc.dnd4e.domain.tracker.common._
import vcc.infra.test.{TransactionalSpecification, TransactionChangeLogger}
import InitiativeTracker.state

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeOrderTest extends JUnit4(InitiativeOrderSpec)

object InitiativeOrderSpec extends Specification with TransactionalSpecification with Mockito {
  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val combC = CombatantID("C")
  val combD = CombatantID("D")
  val combE = CombatantID("E")

  val ioa0 = InitiativeOrderID(combA, 0)
  val ioa1 = InitiativeOrderID(CombatantID("A"), 1)
  val iob = InitiativeOrderID(combB, 0)
  val ioc = InitiativeOrderID(combC, 0)
  val iod = InitiativeOrderID(combD, 0)
  val ioe = InitiativeOrderID(combE, 0)

  shareVariables()

  var aOrder: InitiativeOrder = null

  implicit var aTrans: Transaction = null

  var changeLog: TransactionChangeLogger = null

  val emptyOrder = beforeContext {
    aTrans = new Transaction()
    aOrder = new InitiativeOrder()
    changeLog = new TransactionChangeLogger()
  }

  "an empty InitiatveOrder" ->- (emptyOrder) should {

    "set a simple InitiativeDefinition" in {
      val iDef = InitiativeDefinition(combA, 10, List(14))
      aOrder.setInitiative(iDef)
      aTrans.commit(changeLog)
      changeLog.changes must notBeEmpty
      changeLog.changes.toList must_== List(InitiativeOrderChange(List(InitiativeTracker.initialTracker(ioa0, 14))))
    }

    "not define a head for initiative robin upon add" in {
      val iDef = InitiativeDefinition(combA, 10, List(14))
      aOrder.setInitiative(iDef)
      aTrans.commit(changeLog)

      changeLog.changes must notExist(x => x.isInstanceOf[InitiativeOrderFirstChange])
    }

    "must fail on redefinition on an initative definition" in {
      val iDef = InitiativeDefinition(combA, 10, List(14))
      aOrder.setInitiative(iDef)
      val iDef2 = InitiativeDefinition(combA, 9, List(12))
      aOrder.setInitiative(iDef2) must throwAn[Exception]
    }


    "add compound InitiativeDefinition" in {

      withTransaction {
        trans =>
          val iDef = InitiativeDefinition(combA, 10, List(14, 25))
          aOrder.setInitiative(iDef)(trans)
      } afterCommit {
        changes =>
          changes must notBeEmpty
          changes.toList must_== List(InitiativeOrderChange(List(InitiativeTracker.initialTracker(ioa1, 25), InitiativeTracker.initialTracker(ioa0, 14))))
      } afterUndo {
        changes =>
          extractOrderChange(changes) must_== Nil
      } does "preserve baseList on and undo" afterRedo {
        changes =>
          extractOrderChange(changes) must_== List(ioa1, ioa0)

          aTrans = new Transaction()
          aOrder.setInitiative(InitiativeDefinition(combB, 10, List(17)))
          aTrans.commit(changeLog)
          extractOrderChange(changeLog.changes) must_== List(ioa1, iob, ioa0)
      }
    }

    "preserver random tie break between calls" in {
      // This is needed to make sure that initiative are in order (no random part)
      val iDef1 = InitiativeDefinition(combA, 10, List(14, 14, 14, 14, 14))
      val iDef2 = InitiativeDefinition(combB, 10, List(14, 14, 14, 14, 14))
      aOrder.setInitiative(iDef1)
      aOrder.setInitiative(iDef2) mustNot throwA[Exception]
    }

    "throw exception on a startCombat on an empty robin" in {
      aOrder.startCombat() must throwA[NoSuchElementException]
    }
  }

  val loadedOrder = beforeContext {
    aTrans = new Transaction()
    aOrder = new InitiativeOrder()
    changeLog = new TransactionChangeLogger()

    aOrder.setInitiative(InitiativeDefinition(combA, 10, List(14, 9)))
    aOrder.setInitiative(InitiativeDefinition(combB, 10, List(10)))
    aOrder.setInitiative(InitiativeDefinition(combC, 8, List(18)))
    aOrder.setInitiative(InitiativeDefinition(combD, 4, List(7)))
    aTrans.commit(changeLog)
    aTrans = new Transaction()
  }

  val loadedAndStartedOrder = beforeContext {
    aTrans = new Transaction()
    aOrder = new InitiativeOrder()
    changeLog = new TransactionChangeLogger()

    aOrder.setInitiative(InitiativeDefinition(combA, 10, List(14, 9)))
    aOrder.setInitiative(InitiativeDefinition(combB, 10, List(10)))
    aOrder.setInitiative(InitiativeDefinition(combC, 8, List(18)))
    aOrder.setInitiative(InitiativeDefinition(combD, 4, List(7)))
    aOrder.startCombat()
    aTrans.commit(changeLog)
    aTrans = new Transaction()
  }


  "a loaded InitiativeOrder" ->- (loadedOrder) should {

    "have a container for each Initiative order" in {
      val ioids = Seq(ioa0, ioa1, iob, ioc, iod)
      ioids.foreach(x => aOrder.initiativeTrackerFor(x) must notBeNull)
    }

    "throw exception for non-existant object" in {
      aOrder.initiativeTrackerFor(InitiativeOrderID(combE, 0)) must throwA[NoSuchElementException]
    }

    "throw exception when setRobinPosition" in {
      aOrder.setRobinHead(ioc) must throwA(new IllegalStateException("Combat not started"))
    }

    "not return an added tracker after undoing it's add" in {
      aOrder.setInitiative(InitiativeDefinition(combE, 3, List(5)))
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      aOrder.initiativeTrackerFor(ioe) must throwA[NoSuchElementException]
    }

    "change an InitiativeTracker and propagate" in {
      aOrder.updateInitiativeTrackerFor(ioc, InitiativeTracker(ioc, 1, 0, state.Acting))
      aTrans.commit(changeLog)

      changeLog.changes must contain(InitiativeTrackerChange(InitiativeTracker(ioc, 1, 0, state.Acting)))
    }

    "undo the change of an InitiativeTracker" in {
      aOrder.updateInitiativeTrackerFor(ioc, InitiativeTracker(ioc, 1, 18, state.Acting))
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      changeLog.changes must contain(InitiativeTrackerChange(InitiativeTracker(ioc, 0, 18, InitiativeTracker.state.Waiting)))
    }

    "added tracker with setInitiative" in {
      withTransaction {
        trans => aOrder.setInitiative(InitiativeDefinition(combE, 3, List(5)))(trans)
      } afterCommit {
        changes =>
          aOrder.initiativeTrackerFor(ioe) must_== InitiativeTracker.initialTracker(ioe, 5)
          extractOrderChange(changes) must_== List(ioc, ioa0, iob, ioa1, iod, ioe)
      } afterUndo {
        changes =>
          extractOrderChange(changes) must_== List(ioc, ioa0, iob, ioa1, iod)
          aOrder.initiativeTrackerFor(ioe) must throwA[NoSuchElementException]
      } afterRedoAsInCommit ()
    }
  }

  private def startCombatWrapper() {
    val trans = new Transaction()
    aOrder.startCombat()(trans)
    trans.commit(changeLog)
  }

  private def wrapInTransaction(f: Transaction => Unit): Seq[ChangeNotification] = {
    val trans = new Transaction()
    f(trans)
    trans.commit(changeLog)
    changeLog.changes
  }

  "an InitiativeOrder transactionally" ->- (loadedOrder) should {
    "go to the first combatant on the order on a startCombat" in {
      withTransaction {
        transaction => aOrder.startCombat()(transaction)
      } afterCommit {
        changes => changes must contain(InitiativeOrderFirstChange(ioc))
      } afterUndo {
        changes => changes must contain(InitiativeOrderFirstChange(null))
      } afterRedoAsInCommit ()
    }
  }

  "an InitiativeOrder as a round robin of order entries" ->- (loadedOrder) should {

    "go to the first combatant on the order on a startCombat" in {
      aOrder.startCombat()
      aTrans.commit(changeLog)

      changeLog.changes must contain(InitiativeOrderFirstChange(ioc))
    }

    "fail to rotate if not started" in {
      aOrder.rotate() must throwA[IllegalStateException]
    }

    "undo a startCombat command" in {
      aOrder.startCombat()
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      changeLog.changes must contain(InitiativeOrderFirstChange(null))
    }

    "update the robin when someone is added and propagate" in {
      aOrder.setInitiative(InitiativeDefinition(combE, 3, List(35))) //And 
      aTrans.commit(changeLog)

      extractOrderChange(changeLog.changes) must_== List(ioe, ioc, ioa0, iob, ioa1, iod)
    }

    "undo the robin change for adding an intiative" in {
      aOrder.setInitiative(InitiativeDefinition(combE, 3, List(5)))
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      extractOrderChange(changeLog.changes) must_== List(ioc, ioa0, iob, ioa1, iod)
    }

    "not accept moveBefore if the combat has not started" in {
      aOrder.moveBefore(iod, ioa0) must throwA[IllegalStateException]
    }

    "remove a combatant information from the order" in {
      withTransaction {
        trans =>
          aOrder.removeCombatant(combA)(trans)
      } afterCommit {
        changes =>
          extractOrderChange(changes) must_== List(ioc, iob, iod)
          changes must notContain(InitiativeTrackerChange(null)) //Avoid empty changed
          aOrder.initiativeTrackerFor(ioa0) must throwA[NoSuchElementException]
      } afterUndo {
        changes =>
          extractOrderChange(changes) must_== List(ioc, ioa0, iob, ioa1, iod)
          changes must contain((InitiativeTrackerChange(InitiativeTracker.initialTracker(ioa0, 14))))
          changes must contain((InitiativeTrackerChange(InitiativeTracker.initialTracker(ioa1, 9))))
      } afterRedoAsInCommit ()

    }

    "allow adding the same combatant after it has been removed" in {
      aOrder.removeCombatant(combA)
      aTrans.commit(changeLog)
      aTrans = new Transaction()
      aOrder.setInitiative(InitiativeDefinition(combA, 1, List(2)))
      aTrans.commit(changeLog)

      extractOrderChange(changeLog.changes) must_== List(ioc, iob, iod, ioa0)
      changeLog.changes must notContain(InitiativeTrackerChange(InitiativeTracker.initialTracker(ioa0, 0)))
    }

    "clear the entire order" in {
      aOrder.clearOrder()
      aTrans.commit(changeLog)

      changeLog.changes.toList must_== List((InitiativeOrderChange(Nil)))
    }
  }

  "a loaded InitiativeOrder" ->- (loadedAndStartedOrder) should {

    "not allow the removal of a Combatant " in {
      aOrder.removeCombatant(combA) must throwA[IllegalStateException]
    }

    "rotate the log and indicate this" in {
      aOrder.rotate()
      aTrans.commit(changeLog)

      changeLog.changes must contain(InitiativeOrderFirstChange(ioa0))
    }

    "undo the rotation and propagate" in {
      aOrder.rotate()
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      changeLog.changes must contain(InitiativeOrderFirstChange(ioc))
    }

    "after undo of rotation a futher rotate must return the undone next" in {
      aOrder.rotate()
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      val nTrans = new Transaction()
      aOrder.rotate()(nTrans)
      nTrans.commit(changeLog)
      changeLog.changes must contain(InitiativeOrderFirstChange(ioa0))
    }

    "preserve the reorder internal when adding after a reorder should " in {
      //We need to add fake transaction to make sure we preserve data
      //This transaction will be undone
      aOrder.moveBefore(ioc, iod)
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      aTrans = new Transaction()
      aOrder.moveBefore(iod, ioa0)
      aTrans.commit(changeLog)

      aTrans = new Transaction()
      aOrder.setInitiative(InitiativeDefinition(combE, 3, List(17))) // Add after C
      aTrans.commit(changeLog)
      extractOrderChange(changeLog.changes) must_== List(ioc, ioe, iod, ioa0, iob, ioa1)
    }

    "preserve the reorder internal when reordering after another reorder" in {
      //We need to add fake transaction to make sure we preserve data
      //This transaction will be undone
      aOrder.moveBefore(ioc, iod)
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      // This one is for keeping
      aTrans = new Transaction
      aOrder.moveBefore(iod, ioa0)
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)
      aTrans.redo(changeLog)

      aTrans = new Transaction()
      aOrder.moveBefore(iob, iod)
      aTrans.commit(changeLog)
      extractOrderChange(changeLog.changes) must_== List(ioc, iob, iod, ioa0, ioa1)
    }

    "update the robin when a reorder is done and propagate" in {
      aOrder.moveBefore(iod, ioa0)
      aTrans.commit(changeLog)

      extractOrderChange(changeLog.changes) must_== List(ioc, iod, ioa0, iob, ioa1)
    }

    "not change head on reorder moves to the first slot" in {
      aOrder.moveBefore(iod, ioc)
      aTrans.commit(changeLog)

      extractOrderChange(changeLog.changes) must_== List(iod, ioc, ioa0, iob, ioa1)
      changeLog.changes must notExist(x => x.isInstanceOf[InitiativeOrderFirstChange])
    }

    "undo the reorder robin change and propagate" in {
      aOrder.moveBefore(iod, ioa0)
      aTrans.commit(changeLog)
      aTrans.undo(changeLog)

      extractOrderChange(changeLog.changes) must_== List(ioc, ioa0, iob, ioa1, iod)
    }

    "clear the entire order" in {
      withTransaction {
        trans => aOrder.clearOrder()(trans)
      } afterCommit {
        changes =>
          changes must containAll(List(InitiativeOrderFirstChange(null), InitiativeOrderChange(Nil)))
      } afterUndo {
        changes =>
          changes must contain(InitiativeOrderFirstChange(ioc))
          extractOrderChange(changeLog.changes) must_== List(ioc, ioa0, iob, ioa1, iod)
          List(ioa0, ioa1, iob, ioc, iod).foreach(e => changes must notContain(InitiativeTrackerChange(InitiativeTracker.initialTracker(e, 0))))
      } afterRedo {
        changes =>
          changes must containAll(List(InitiativeOrderFirstChange(null), InitiativeOrderChange(Nil)))
          List(ioa0, ioa1, iob, ioc, iod).foreach {
            e =>
              aOrder.initiativeTrackerFor(e) must throwA[NoSuchElementException]
          }
      }
    }
    "setRobinHead if element is in Robin" in {
      withTransaction {
        trans =>
          aOrder.setRobinHead(iod)(trans)
      } afterCommit {
        changes =>
          changes must contain(InitiativeOrderFirstChange(iod))
      } afterUndo {
        changes =>
          changes must contain(InitiativeOrderFirstChange(ioc))
      } afterRedoAsInCommit ()
    }

    "throw NoSuchElement if setRobinHead is called on element not in Robin" in {
      aOrder.setRobinHead(ioe) must throwA[NoSuchElementException]
    }

    "allow adding a cleared combatant after clearOrder" in {
      // Make sure we cleared inner structures
      aOrder.clearOrder()
      aOrder.setInitiative(InitiativeDefinition(combC, 3, List(17))) mustNot throwA[Exception]
    }

    "allow adding a cleared combatant after clearOrder after a moveBefore" in {
      // Make sure we cleared inner structures
      aOrder.moveBefore(ioc, iod)
      aOrder.clearOrder()
      aOrder.setInitiative(InitiativeDefinition(combC, 3, List(17))) mustNot throwA[Exception]
    }

    "end combat when its started (same as clear)" in {
      withTransaction {
        trans =>
          aOrder.clearOrder()(trans)
      } afterCommit {
        changes =>
          changes must contain(InitiativeOrderFirstChange(null))
          changes must contain(InitiativeOrderChange(Nil))
      } afterUndo {
        changes =>
          changes must contain(InitiativeOrderFirstChange(ioc))
          extractOrderChange(changes) must_== List(ioc, ioa0, iob, ioa1, iod)

      } afterRedoAsInCommit ()
    }


    "end combat when its started (same as clear)" in {
      val trans = new Transaction()

      aOrder.clearOrder()(trans)
      trans.commit(changeLog)
      trans.undo(changeLog)
      trans.redo(changeLog)
      trans.undo(changeLog)
      trans.redo(changeLog)
      0 must_== 0
    }
  }

  def extractOrderChange(changes: Seq[ChangeNotification]): List[InitiativeOrderID] = {
    val orderChange = changes.find(x => x.isInstanceOf[InitiativeOrderChange]).get.asInstanceOf[InitiativeOrderChange]
    if (orderChange != null) {
      orderChange.order.map(x => x.orderID)
    } else Nil
  }
}