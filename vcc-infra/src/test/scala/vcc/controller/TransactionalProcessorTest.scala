/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.controller

import message.{TrackerChanged, TransactionalAction}
import scala.collection.mutable.Queue
import transaction.{Transaction, ChangeNotification}
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope

case class DummyAction(sub: Symbol) extends TransactionalAction {
  def description(): String = "Run " + sub
}

class TransactionalProcessorTest extends SpecificationWithJUnit with Mockito {

  trait context extends Scope {
    protected val actionTwice = DummyAction('TWICE)
    protected val actionOnce = DummyAction('DOIT)
    protected val ruling1 = YesNoRuling("One?")
    protected val ruling2 = YesNoRuling("Two?")
    protected val mQueue = spy(new Queue[TransactionalAction])

    // This is a dummy rule, will not match
    protected val tailRewriteRule = mock[PartialFunction[TransactionalAction, Seq[TransactionalAction]]]
    tailRewriteRule.isDefinedAt(any) returns false

    // Ruling search will by default not match
    protected val firstRulingSearch = mock[PartialFunction[TransactionalAction, List[PendingRuling[List[TransactionalAction]]]]]
    protected val secondRulingSearch = mock[PartialFunction[TransactionalAction, List[PendingRuling[List[TransactionalAction]]]]]
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
    protected val aSource = mock[CommandSource]
    protected val pendingRuling1 = mock[PendingRuling[List[TransactionalAction]]]
    protected val pendingRuling2 = mock[PendingRuling[List[TransactionalAction]]]
    pendingRuling1.processDecision(any) returns Some(Nil)
    pendingRuling1.ruling returns ruling1
    pendingRuling2.processDecision(any) returns Some(Nil)
    pendingRuling2.ruling returns ruling2
    protected val answer1true = YesNoDecision(ruling1, true)
    protected val answer2true = YesNoDecision(ruling2, true)
    protected val theRulings = List(ruling1, ruling2)
    protected val thePendingRuling = List(pendingRuling1, pendingRuling2)
  }

  "aProcessor" should {

    "must enqueue action with default processor" in new context {
      val action = DummyAction('DOIT)
      aProcessor.dispatch(new Transaction(), aSource, action)
      there was one(mQueue).+=(action)
    }

    "must call all rewrite functions" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was one(tailRewriteRule).isDefinedAt(any)
    }

    "must apply a rewrite if there is a match" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionTwice)
      there was atLeastTwo(mQueue).+=(actionOnce)
    }

    "must not call rewrite functions after a match" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionTwice)
      there was no(tailRewriteRule).isDefinedAt(any)
    }

    "call registered ruling locators in order" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was one(firstRulingSearch).isDefinedAt(any) then
        one(secondRulingSearch).isDefinedAt(any)
    }

    "not ask CommandSource if there are no ruling" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      there was no(aSource).provideDecisionsForRulings(any, any)
    }

    "generate a list all pending rulings and ask commandSource" in new context {
      firstRulingSearch.isDefinedAt(actionOnce) returns true
      firstRulingSearch.apply(actionOnce) returns List(pendingRuling1)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(pendingRuling2)
      aSource.provideDecisionsForRulings(actionOnce, List(ruling1, ruling2)) returns List(answer1true, answer2true)

      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      there was one(firstRulingSearch).apply(actionOnce) then
        one(secondRulingSearch).apply(actionOnce) then
        one(aSource).provideDecisionsForRulings(actionOnce, theRulings)

      aProcessor.allExecutedActions must_== List('DOIT)
    }

    "add actions generated by question on queue prior to rewrite" in new context {
      firstRulingSearch.isDefinedAt(actionOnce) returns true
      firstRulingSearch.apply(actionOnce) returns List(pendingRuling1)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(pendingRuling2)
      aSource.provideDecisionsForRulings(actionOnce, List(ruling1, ruling2)) returns List(answer1true, answer2true)
      pendingRuling1.processDecision(answer1true) returns Some(List(DummyAction('TenTimes), DummyAction('Once)))
      pendingRuling2.processDecision(answer2true) returns Some(List(DummyAction('yes)))

      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      aProcessor.allExecutedActions must_== List('TenTimes, 'Once, 'yes, 'DOIT)
    }

    "clear queues on exception within question execution" in new context {
      val toFailAction = DummyAction('THROW)
      secondRulingSearch.isDefinedAt(actionOnce) returns true
      secondRulingSearch.apply(actionOnce) returns List(pendingRuling2)
      aSource.provideDecisionsForRulings(actionOnce, List(ruling2)) returns List(answer2true)
      pendingRuling2.processDecision(answer2true) returns Some(List(toFailAction))

      aProcessor.dispatch(new Transaction(), aSource, actionOnce) must throwAn[IllegalStateException]
      aProcessor.allExecutedActions must_== Nil
    }

    "enqueue to last position while handling a question action" in new context {
      val repeatDecision = DummyAction('AGAIN)
      secondRulingSearch.isDefinedAt(actionOnce) returns true thenReturns false
      secondRulingSearch.apply(actionOnce) returns List(pendingRuling2)
      aSource.provideDecisionsForRulings(actionOnce, List(ruling2)) returns List(answer2true)
      pendingRuling2.processDecision(answer2true) returns Some(List(DummyAction('One), repeatDecision))
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)

      aProcessor.allExecutedActions must_== List('One, 'AGAIN, 'DOIT, 'DOIT)
    }

    "only execute base action when no ruling was required" in new context {
      aProcessor.dispatch(new Transaction(), aSource, actionOnce)
      aProcessor.allExecutedActions must_== List('DOIT)
    }

    "must enqueue a action if handler asks it" in new context {
      val actionAgain = DummyAction('AGAIN)
      aProcessor.dispatch(new Transaction(), aSource, actionAgain)
      there was two(mQueue).+=(actionAgain)
      there was two(mQueue).dequeue()
    }.pendingUntilFixed("Should work")

    "dequeue messages when dispatching" in new context {
      aProcessor.dispatch(new Transaction(), aSource, DummyAction('DOIT))
      there was one(mQueue).dequeue()
    }

    "throw UnhandledActionException when message is not processed" in new context {
      val action = DummyAction('UNKNOWN)
      aProcessor.dispatch(new Transaction(), aSource, action) must throwA(new UnhandledActionException(action))
    }

    "flush queue when processing an action throws an exception" in new context {
      aProcessor.dispatch(new Transaction(), aSource, DummyAction('THROW)) must throwAn[Exception]
      there was one(mQueue).clear()
    }
  }

  "TransactionalProcessor queryCommandSource" should {
    val mAction = mock[TransactionalAction]

    "send rulings to source and return something on a valid answer" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer1true, answer2true)
      ruling1.isValidDecision(answer1true) must beTrue
      ruling2.isValidDecision(answer2true) must beTrue

      val ret = aProcessor.queryCommandSource(mAction, aSource, thePendingRuling)
      ret must not beNull;
      there was one(aSource).provideDecisionsForRulings(mAction, theRulings)
    }

    "throw some MissingDecisionException when answer are in the wrong order" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer2true, answer1true)
      aProcessor.queryCommandSource(mAction, aSource, thePendingRuling) must throwA(new MissingDecisionException(ruling1))
    }

    "throw some MissingDecisionException when not all have been answered" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer1true)
      aProcessor.queryCommandSource(mAction, aSource, thePendingRuling) must throwA(new MissingDecisionException(ruling2))
    }

    "throw some MissingDecisionException when last has not been answered" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer1true)
      aProcessor.queryCommandSource(mAction, aSource, thePendingRuling) must throwA(new MissingDecisionException(ruling2))
    }


    "evoke the action generation for each question and return actions" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer1true, answer2true)

      val ret = aProcessor.queryCommandSource(mAction, aSource, thePendingRuling)
      ret must_== Nil

      there was one(pendingRuling1).processDecision(answer1true) then
        one(pendingRuling2).processDecision(answer2true)

    }
    "append generated action in order into a single list" in new context {
      aSource.provideDecisionsForRulings(mAction, theRulings) returns List(answer1true, answer2true)
      pendingRuling1.processDecision(answer1true) returns Some(List(new DummyAction('Ruled10)))
      pendingRuling2.processDecision(answer2true) returns Some(List(new DummyAction('RuledYup)))
      val ret = aProcessor.queryCommandSource(mAction, aSource, thePendingRuling)

      ret.size must_== 2
      ret contains (new DummyAction('Ruled10))
      ret contains (new DummyAction('RuledYup))
    }
  }
}