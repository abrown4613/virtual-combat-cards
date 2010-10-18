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


import message.{TrackerChanged, TransactionalAction}
import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import scala.collection.mutable.Queue
import transaction.{Transaction, ChangeNotification}

@RunWith(classOf[JUnitSuiteRunner])
class TransactionalProcessorTest extends JUnit4(TransactionalProcessorSpec)

case class DummyAction(sub: Symbol) extends TransactionalAction {
  def description: String = "Run " + sub
}

object TransactionalProcessorSpec extends Specification with Mockito {
  val mQueue = spy(new Queue[TransactionalAction])
  val aProcessor = new TransactionalProcessor[String]("data", mQueue) {
    addHandler {
      case DummyAction('THROW) =>
        throw new IllegalStateException("Cant handle")
      case DummyAction('DOIT) =>
      case DummyAction('AGAIN) =>
        enqueueAction(DummyAction('DOIT))
    }

    addRewriteRule {
      case DummyAction('TWICE) => Seq(DummyAction('DOIT), DummyAction('DOIT))
    }

    def publish(changes: Seq[ChangeNotification]): TrackerChanged = TrackerChanged(changes.toList)
  }
  val aSource = mock[CommandSource]

  "aProcessor" should {

    "must enqueue action with default processor" in {
      val action = DummyAction('DOIT)
      aProcessor.dispatch(new Transaction(), aSource, action)
      there was one(mQueue).+=(action)
    }

    "must apply a rewrite if there is a match" in {
      val actionTwice = DummyAction('TWICE)
      val actionOnce = DummyAction('DOIT)
      aProcessor.dispatch(new Transaction(), aSource, actionTwice)
      there was one(mQueue).+=(actionOnce) then
              one(mQueue).+=(actionOnce)
    }

    "must enqueue a action if handler asks it" in {
      val actionAgain = DummyAction('AGAIN)
      val actionOnce = DummyAction('DOIT)
      aProcessor.dispatch(new Transaction(), aSource, actionAgain)
      there was one(mQueue).+=(actionAgain) then
              one(mQueue).dequeue() then // after the dequeue
              one(mQueue).+=(actionOnce)
    }

    "dequeue messages when dispatching" in {
      aProcessor.dispatch(new Transaction(), aSource, DummyAction('DOIT))
      there was one(mQueue).dequeue()
    }

    "throw UnhandledActionException when message is not processed" in {
      val action = DummyAction('UNKNOWN)
      aProcessor.dispatch(new Transaction(), aSource, action) must throwA(new UnhandledActionException(action))
    }

    "flush queue when processing an action throws an exception" in {
      aProcessor.dispatch(new Transaction(), aSource, DummyAction('THROW)) must throwAn[Exception]
      there was one(mQueue).clear()
    }
  }
}