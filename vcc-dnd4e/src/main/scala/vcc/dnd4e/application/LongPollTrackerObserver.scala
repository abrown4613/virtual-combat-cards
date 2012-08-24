/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import vcc.tracker.Tracker
import concurrent.SyncVar

class LongPollTrackerObserver[S] extends Tracker.Observer[S] {

  private var waiting: List[SyncVar[S]] = Nil

  def waitingForState(syncVar: SyncVar[S]) {
    synchronized {
      waiting = syncVar :: waiting
    }
  }

  def stateUpdated(n: S) {
    takeListAndClear().foreach(w => w.put(n))
  }

  private def takeListAndClear(): List[SyncVar[S]] =
    synchronized {
      val t = waiting
      waiting = Nil
      t
    }
}