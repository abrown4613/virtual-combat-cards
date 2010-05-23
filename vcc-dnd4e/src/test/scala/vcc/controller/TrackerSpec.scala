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
package vcc.controller

import message._
import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import org.mockito.Matchers._
import actors.Actor
import collection.mutable.ArrayBuffer
import transaction._

@RunWith(classOf[JUnitSuiteRunner])
class TrackerTest extends JUnit4(TrackerSpec)

object TrackerSpec extends Specification with Mockito {
  val mController = mock[TrackerController]
  val mLog = mock[TransactionLog[TransactionalAction]]
  val mObserver = mock[Actor]
  val tracker = new Tracker(mController, mLog) {
    override def scheduler = new scala.actors.SingleThreadedScheduler
  }

  case class MyChange(msg: String) extends ChangeNotification

  tracker.start

  "Tracker" should {

    "dispatch message to TrackerController" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]

      tracker ! Command(src, msg)

      there was one(mController).dispatch(any[Transaction], refEq(src), refEq(msg))
    }

    "notify CommandSource on success" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]

      msg.description returns "an action"

      tracker ! Command(src, msg)

      there was one(src).actionCompleted("an action", false)
      there was no(src).actionCancelled(any[String])
    }

    "notify CommandSource on success with changes" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]

      mController.dispatch(any[Transaction], any[CommandSource], any[TransactionalAction]) answers {
        a =>
          val t = a.asInstanceOf[Array[Any]](0).asInstanceOf[Transaction]
          //Need to mock a simple UndoMemento
          val um = mock[UndoMemento[Int]]
          um.changeNotification returns None
          t.addMemento(um)
      }

      msg.description returns "an action"

      tracker ! Command(src, msg)

      there was one(src).actionCompleted("an action", true)
      there was no(src).actionCancelled(any[String])
    }

    "notify CommandSource of failure with exception" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]
      mController.dispatch(any[Transaction], refEq(src), refEq(msg)) throws new RuntimeException("boom!")

      tracker ! Command(src, msg)

      there was one(mController).dispatch(any[Transaction], refEq(src), refEq(msg))
      there was no(src).actionCompleted(any[String], any[Boolean])
      there was one(src).actionCancelled("boom!")
    }

    "call change publisher to process changes" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]

      mController.publish(Nil) returns TrackerChanged(List(MyChange("10")))

      tracker ! Command(src, msg)

      there was one(mController).publish(new ArrayBuffer()) //This is needed since there are boxing issues in this
    }

    "save transaction to log when it has something" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]
      val myData = new Undoable[Int](10, x => new ChangeNotification() {})

      mController.dispatch(any[Transaction], refEq(src), refEq(msg)) answers {
        p =>
          implicit val t = p.asInstanceOf[Seq[Any]](0).asInstanceOf[Transaction]
          myData.value = 11
      }

      tracker ! Command(src, msg)

      there was one(mLog).store(refEq(msg), any[Transaction])
    }

    "not save empty transactions" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]


      tracker ! Command(src, msg)

      there was no(mLog).store(refEq(msg), any[Transaction])
    }

    "clear transaction log" in {
      tracker ! ClearTransactionLog()
      there was one(mLog).clear
    }
  }

  "tracker with observers" ->- (beforeContext {
    tracker ! AddObserver(mObserver)
  }) should {

    "send changes to observers" in {
      val msg = mock[TransactionalAction]
      val src = mock[CommandSource]

      mController.publish(new ArrayBuffer()) returns TrackerChanged(List(MyChange("11")))

      tracker ! Command(src, msg)

      there was one(mController).publish(new ArrayBuffer()) //This is needed since there are boxing issues in this
      there was one(mObserver) ! TrackerChanged(List(MyChange("11")))
    }

    "silently do nothing if roll forward is out of bounds not publishing" in {
      mLog.rollforward(tracker) throws new TransactionLogOutOfBounds("bla")

      tracker ! Redo()

      there was one(mLog).rollforward(tracker)
      there was no(mObserver) ! any[Any]
    }

    "ask TransactionLog to Redo and publish " in {
      mLog.rollforward(tracker) answers {
        pub => pub.asInstanceOf[TransactionChangePublisher].publishChange(new ArrayBuffer)
      }
      mController.publish(new ArrayBuffer()) returns TrackerChanged(List(MyChange("Rolled forward")))
      tracker ! Redo()

      there was one(mLog).rollforward(tracker)
      there was one(mObserver) ! TrackerChanged(List(MyChange("Rolled forward")))
    }

    "ask TransactionLog to Undo and publish " in {
      mLog.rollback(tracker) answers {
        pub => pub.asInstanceOf[TransactionChangePublisher].publishChange(new ArrayBuffer)
      }
      mController.publish(new ArrayBuffer()) returns TrackerChanged(List(MyChange("Rolled back")))
      tracker ! Undo()

      there was one(mLog).rollback(tracker)
      there was one(mObserver) ! TrackerChanged(List(MyChange("Rolled back")))
    }

    "silently do nothing if roll back is out of bounds not publishing " in {
      mLog.rollback(tracker) throws new TransactionLogOutOfBounds("bla")

      tracker ! Undo()

      there was one(mLog).rollback(tracker)
      there was no(mObserver) ! any[Any]
    }
  }

}