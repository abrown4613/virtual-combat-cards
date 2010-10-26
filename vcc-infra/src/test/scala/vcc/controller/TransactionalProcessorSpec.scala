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
  private val actionTwice = DummyAction('TWICE)
  private val actionOnce = DummyAction('DOIT)
  private val mQueue = spy(new Queue[TransactionalAction])

  // This is a dummy rule, will not match
  private val tailRewriteRule = mock[PartialFunction[TransactionalAction, Seq[TransactionalAction]]]
  tailRewriteRule.isDefinedAt(any) returns false

  // Ruling search will by default not match
  private val firstRulingSearch = mock[PartialFunction[TransactionalAction, List[Ruling[_]]]]
  private val secondRulingSearch = mock[PartialFunction[TransactionalAction, List[Ruling[_]]]]
  firstRulingSearch.isDefinedAt(any) returns false
  secondRulingSearch.isDefinedAt(any) returns false

  val aProcessor = new TransactionalProcessor[String]("data", mQueue) {
    private var executedActions: List[Symbol] = Nil
    addHandler {
      case DummyAction('THROW) =>
        throw new IllegalStateException("Cant handle")
      case DummyAction('DOIT) =>
      case DummyAction('AGAIN) =>
        enqueueAction(DummyAction('DOIT))
    }

    addHandler {
      case DummyAction(verb) if (verb != 'UNKNOWN) =>
        executedActions = verb :: executedActions
    }

    addRewriteRule {
      case DummyAction('TWICE) => Seq(DummyAction('DOIT), DummyAction('DOIT))
    }
    addRewriteRule(tailRewriteRule)

    addRulingSearch(firstRulingSearch)
    addRulingSearch(secondRulingSearch)

    def publish(changes: Seq[ChangeNotification]): TrackerChanged = TrackerChanged(changes.toList)

    def allExecutedActions = executedActions.reverse
  }
  private val aSource = mock[CommandSource]

  private val mRuling1 = mock[Ruling[Int]]
  private val mRuling2 = mock[Ruling[String]]
  mRuling1.generateActions(any) returns Nil
  mRuling2.generateActions(any) returns Nil
  private val answer1 = new Decision(10, mRuling1)
  private val answer2 = new Decision("yup", mRuling2)
  private val theRulings = List(mRuling1, mRuling2)

  "aProcessor" should {

    "must enqueue action with default processor" in {
      val action = DummyAction('DOIT)
      aProcessor.dispatch(new Transaction(), aSource, action)
      there was one(mQueue).+=(action)
    }

    "must call all rewrite functions" in {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was one(tailRewriteRule).isDefinedAt(any)
    }

    "must apply a rewrite if there is a match" in {
      aProcessor.dispatch(new Transaction(), aSource, actionTwice)
      there was one(mQueue).+=(actionOnce) then
              one(mQueue).+=(actionOnce)
    }

    "must not call rewrite functions after a match" in {
      aProcessor.dispatch(new Transaction(), aSource, actionTwice)
      there was no(tailRewriteRule).isDefinedAt(any)
    }

    "call registered ruling locators in order" in {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was one(firstRulingSearch).isDefinedAt(any) then
              one(secondRulingSearch).isDefinedAt(any)
    }

    "not ask CommandSource if there are no ruling" in {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was no(aSource).provideDecisionsForRulings(any)
    }

    "generate a list all pending rulings and ask commandSource" in {
      firstRulingSearch.isDefinedAt(actionOnce) returns true
      firstRulingSearch.apply(actionOnce) returns List(mRuling1)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(mRuling2)
      aSource.provideDecisionsForRulings(List(mRuling1, mRuling2)) returns List(answer1, answer2)

      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      there was one(firstRulingSearch).apply(actionOnce) then
              one(secondRulingSearch).apply(actionOnce) then
              one(aSource).provideDecisionsForRulings(theRulings)

      aProcessor.allExecutedActions must_== List('DOIT)
    }

    "add actions generated by decision on queue prior to rewrite" in {
      firstRulingSearch.isDefinedAt(actionOnce) returns true
      firstRulingSearch.apply(actionOnce) returns List(mRuling1)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(mRuling2)
      aSource.provideDecisionsForRulings(List(mRuling1, mRuling2)) returns List(answer1, answer2)
      mRuling1.generateActions(10) returns List(DummyAction('TenTimes), DummyAction('Once))
      mRuling2.generateActions("yup") returns List(DummyAction('yes))

      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      aProcessor.allExecutedActions must_== List('TenTimes, 'Once, 'yes, 'DOIT)
    }

    "clear queues on exception within decision execution" in {
      val toFailAction = DummyAction('THROW)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(mRuling2)
      aSource.provideDecisionsForRulings(List(mRuling2)) returns List(answer2)
      mRuling2.generateActions("yup") returns List(toFailAction)

      aProcessor.dispatch(new Transaction(), aSource, actionOnce) must throwAn[IllegalStateException]
      aProcessor.allExecutedActions must_== Nil
    }

    "enqueue to last position while handling a decision action" in {
      val repeatDecision = DummyAction('AGAIN)
      secondRulingSearch.isDefinedAt(actionOnce) returns true thenReturns false
      secondRulingSearch.apply(actionOnce) returns List(mRuling2)
      aSource.provideDecisionsForRulings(List(mRuling2)) returns List(answer2)
      mRuling2.generateActions("yup") returns List(DummyAction('One), repeatDecision)

      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      aProcessor.allExecutedActions must_== List('One, 'AGAIN, 'DOIT, 'DOIT)
    }

    "only execute base action when no ruling was required" in {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      aProcessor.allExecutedActions must_== List('DOIT)
    }

    "must enqueue a action if handler asks it" in {
      val actionAgain = DummyAction('AGAIN)
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

  "TransactionalProcessor queryCommandSource" should {

    "send rulings to source and return something on a valid answer" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer1, answer2)
      val ret = aProcessor.queryCommandSource(aSource, theRulings)
      ret mustNot beNull
      there was one(aSource).provideDecisionsForRulings(theRulings)
    }

    "throw some MissingDecisionException when answer are in the wrong order" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer2, answer1)
      aProcessor.queryCommandSource(aSource, theRulings) must throwA(new MissingDecisionException(mRuling1))
    }

    "throw some MissingDecisionException when not all have been answered" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer2)
      aProcessor.queryCommandSource(aSource, theRulings) must throwA(new MissingDecisionException(mRuling1))
    }

    "throw some MissingDecisionException when last has not been answered" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer1)
      aProcessor.queryCommandSource(aSource, theRulings) must throwA(new MissingDecisionException(mRuling2))
    }


    "evoke the action generation for each question and return actions" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer1, answer2)

      val ret = aProcessor.queryCommandSource(aSource, theRulings)
      ret must_== Nil

      there was one(mRuling1).generateActions(any) then
              one(mRuling2).generateActions(any)

    }
    "append generated action in order into a single list" in {
      aSource.provideDecisionsForRulings(theRulings) returns List(answer1, answer2)
      mRuling1.generateActions(10) returns List(new DummyAction('Ruled10))
      mRuling2.generateActions("yup") returns List(new DummyAction('RuledYup))

      val ret = aProcessor.queryCommandSource(aSource, theRulings)

      ret.size must_== 2
      ret contains (new DummyAction('Ruled10))
      ret contains (new DummyAction('RuledYup))
    }
  }
}