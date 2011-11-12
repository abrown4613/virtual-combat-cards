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

import message.{TrackerChanged, AddObserver}
import actors.Actor
import transaction.ChangeNotification
import reflect.Manifest
import actors.scheduler.SingleThreadedScheduler
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope

class TrackerChangeObserverTest extends SpecificationWithJUnit with Mockito {

  trait actorContext extends Scope {
    val mBuilder = mock[SnapshotBuilder[String]]
    val mTracker = mock[Actor]
    val anObserverActor = new TrackerChangeObserverActor(mBuilder) {
      override def scheduler = new SingleThreadedScheduler
    }
    anObserverActor.start()
  }

  trait observerWithMockActor extends Scope {
    val mBuilder = mock[SnapshotBuilder[String]]
    val anObserverActor = mock[TrackerChangeObserverActor[String]]
    val mTracker = mock[Actor]
    val anObserver = new TrackerChangeObserver[String](mBuilder, mTracker, anObserverActor)
  }


  "a TrackerChangeObverserActor" should {
    "register itself with the tracker" in new actorContext {
      anObserverActor.registerWithTracker(mTracker)

      there was one(mTracker) ! AddObserver(anObserverActor)
    }

    "send a single change to builder bounded by beginChange and endChange" in new actorContext {
      val change = mock[ChangeNotification]
      anObserverActor ! TrackerChanged(List(change))

      there was one(mBuilder).beginChanges then
        one(mBuilder).processChange(change) then
        one(mBuilder).endChanges
    }

    "capture exception in even processing" in new actorContext {
      val change = mock[ChangeNotification]
      val except = new RuntimeException("Bad move")

      mBuilder.processChange(change) throws except

      anObserverActor ! TrackerChanged(List(change))

      there was one(mBuilder).beginChanges then
        one(mBuilder).processChange(change)
      there was no(mBuilder).endChanges
      there was one(mBuilder).handleFailure(except, List(change))
    }

    "send all changes to builder" in new actorContext {
      val c1 = mock[ChangeNotification]
      val c2 = mock[ChangeNotification]
      val c3 = mock[ChangeNotification]
      anObserverActor ! TrackerChanged(List(c1, c2, c3))

      there was one(mBuilder).beginChanges then
        one(mBuilder).processChange(c1) then
        one(mBuilder).processChange(c2) then
        one(mBuilder).processChange(c3) then
        one(mBuilder).endChanges
    }

    "call registered call backs when change ends" in new actorContext {
      //Register callback
      val aware1 = mock[TrackerChangeAware[String]]
      val aware2 = mock[TrackerChangeAware[String]]
      val change = mock[ChangeNotification]
      val manif = getManifest[String]()

      mBuilder.getSnapshot() returns "Hello"

      anObserverActor ! TrackerChangeObserver.RegisterCallback(aware1, manif)
      anObserverActor ! TrackerChangeObserver.RegisterCallback(aware2, manif)
      anObserverActor ! TrackerChanged(List(change))


      there was one(mBuilder).endChanges then
        one(aware1).snapshotChanged("Hello")
      there was one(aware2).snapshotChanged("Hello")
    }
    "return None if the snapshot generation fails" in new actorContext {
      mBuilder.getSnapshot() answers {
        x =>
          throw new Exception("get the stack")
      }

      (anObserverActor !? TrackerChangeObserver.GetSnapshot) must_== None
    }

    "return Some(snap) if the snapshot generation fails" in new actorContext {
      mBuilder.getSnapshot() returns "Click!"
      (anObserverActor !? TrackerChangeObserver.GetSnapshot) must_== Some("Click!")
    }

    "not register wrong observer callback" in new actorContext {
      val manif = getManifest[Int]()
      val aware1 = mock[TrackerChangeAware[Int]]
      val change = mock[ChangeNotification]
      mBuilder.getSnapshot() returns "Hello"

      anObserverActor ! TrackerChangeObserver.RegisterCallback(aware1, manif)
      anObserverActor ! TrackerChanged(List(change))

      there was no(aware1).snapshotChanged(any[Int])
    }
  }

  //FIXME This need to pass or go. Since 2.9.0-1 will not work.
  "a TrackerChangeObserver" should {
    "on construction start and register" in new observerWithMockActor {
      there was one(anObserverActor).start() then
        one(anObserverActor).registerWithTracker(mTracker)
    }.pendingUntilFixed("Actor mocking in this case fails")
    /*
        "forward snapshot request to actor" in new observerWithMockActor {
          (anObserverActor.!?(TrackerChangeObserver.GetSnapshot)).returns(Some("Click!"))
          anObserver.getSnapshot() must_== "Click!"
          there was no(mBuilder).getSnapshot()
          there was one(anObserverActor).!?(TrackerChangeObserver.GetSnapshot)
        }


        "throw exception if snapshot request returns None" in new observerWithMockActor {
          anObserverActor.!?(TrackerChangeObserver.GetSnapshot) returns None
          anObserver.getSnapshot() must throwAn[Exception]
        }

        "throw exception if snapshot request returns wrong type" in new observerWithMockActor {
          anObserverActor.!?(TrackerChangeObserver.GetSnapshot) returns Some(10)
          anObserver.getSnapshot() must throwAn[ClassCastException]
        }

        "register an observer with the actor" in new observerWithMockActor {
          val aware = mock[TrackerChangeAware[String]]
          val manif = getManifest[String]()
          anObserver.addChangeObserver(aware)
          there was one(anObserverActor).!(TrackerChangeObserver.RegisterCallback(aware, manif))
        }
    */
  }

  def getManifest[S]()(implicit m: Manifest[S]): Manifest[_] = m
}
