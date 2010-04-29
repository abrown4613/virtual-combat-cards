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

import scala.actors.Actor
import scala.actors.Actor.{loop, react}
import message.{TrackerChanged, AddObserver}
import transaction.ChangeNotification
import reflect.Manifest

/**
 * A snapshot builder is responsible for collecting changes to a combat state as published by the tracker and applying
 * them to it's vision of the state. It must also provide read-only snapshots of it's vision of the state.
 */
trait SnapshotBuilder[S] {

  /**
   * This method is called by the driving TrackerChangeObserver to indicate it is about to start sending changes. This
   * may be used to collect changes into some kind of list.
   */
  def beginChanges()

  /**
   * Called by the driving TrackerChangeObserver for each change received from the tracker.
   */
  def processChange(change: ChangeNotification)

  /**
   *  Called to indicate that all changes sent by the tracker have been sent.
   */
  def endChanges()

  /**
   * Produce a read-only snapshot of the builders view.
   * @return A snapshot
   */
  def getSnapshot(): S
}


/**
 * This is the actor responsible for receiving changes from the tracker and posting them to the builder.
 * It also allows for a serialized request of a snapshot.
 */
private[controller] class TrackerChangeObserverActor[S](builder: SnapshotBuilder[S])(implicit val manifest: Manifest[AnyRef]) extends Actor {
  private var callbacks: List[TrackerChangeAware[S]] = Nil

  def act() = {
    loop {
      react {
        case TrackerChangeObserver.GetSnapshot =>
          try {
            reply(Some(builder.getSnapshot()))
          } catch {
            case e =>
              reply(None)
          }
        case TrackerChanged(changes) =>
          builder.beginChanges()
          changes.foreach(builder.processChange)
          builder.endChanges()
          callbacks.foreach(cb => cb.snapshotChanged(builder.getSnapshot()))

        case TrackerChangeObserver.RegisterCallback(cb, manif) if (manif <:< manifest) =>
          callbacks = cb.asInstanceOf[TrackerChangeAware[S]] :: callbacks

        case TrackerChangeObserver.RegisterCallback(cb, manif) if (!(manif <:< manifest)) =>
        //
      }
    }
  }

  def registerWithTracker(tracker: Actor) {
    tracker ! AddObserver(this)
  }
}

/**
 * This interface allows object to be informed bty the TrackerChangeObserver when a set of changes have
 * been applied and a new Snapshot is available.
 */
trait TrackerChangeAware[S] {

  /**
   * Callback for a snapshot change
   */
  def snapshotChanged(s: S)
}

object TrackerChangeObserver {
  /**
   *   Message used to get a snapshot
   */
  case object GetSnapshot

  case class RegisterCallback[S](callback: TrackerChangeAware[S], manifest: Manifest[_])
}

/**
 * This is a adapter to a TrackerChangeObserverActor, it hides the actor processes behind a regular synchronous object.
 */
class TrackerChangeObserver[S](builder: SnapshotBuilder[S], tracker: Actor, observer: TrackerChangeObserverActor[S])(implicit manifest: Manifest[S]) {

  // def this(builder: SnapshotBuilder[S], tracker: Actor, observer: TrackerChangeObserverActor[S])(implicit m: Manifest[S]) = this(builder,tracker,observer,m)

  /**
   * This is the constructor to be used normally.
   * @param builder A SnapshotBuilder that will handle change messages
   * @param tracker The tracker it should register with
   */
  def this(builder: SnapshotBuilder[S], tracker: Actor)(implicit m: Manifest[S]) = this (builder, tracker, new TrackerChangeObserverActor[S](builder))

  //Initialization
  observer.start()
  observer.registerWithTracker(tracker)

  /**
   * Returns a snapshot of the current state.
   * @return A snapshot
   * @throw Exception When the building of a snapshot fails.
   */
  def getSnapshot(): S = observer !? TrackerChangeObserver.GetSnapshot match {
    case Some(s: S) => s
    case None => throw new Exception("Failed to generate snapshot")
  }

  /**
   * Adds an observer to the internal actor, so that the observer will be called when the snapshot changes
   * @param obs Observer that wishes to receive notification for the snapshot change.
   */
  def addChangeObserver(obs: TrackerChangeAware[S]) {
    observer ! TrackerChangeObserver.RegisterCallback(obs, manifest)
  }
}
