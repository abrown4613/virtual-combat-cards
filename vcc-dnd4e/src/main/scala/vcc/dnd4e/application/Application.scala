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
package vcc.dnd4e.application

import vcc.dnd4e.tracker.common.CombatState
import java.lang.String
import vcc.tracker.Tracker
import vcc.dnd4e.tracker.dispatcher.InterimController
import vcc.dnd4e.tracker.common.Command.SetCombatComment
import java.io.{FileInputStream, FileOutputStream, File}

object Application {
  private var theInstance: Application = null

  private val theLongPollObserver = new LongPollTrackerObserver[CombatState]()

  def getInstance: Application = theInstance

  def getLongPollObserver = theLongPollObserver

  def initialize(observer: Tracker.Observer[CombatState]) {
    val tracker = new Tracker[CombatState](new InterimController())
    initialize(tracker, observer)
  }

  def initialize(tracker: Tracker[CombatState], observer: Tracker.Observer[CombatState]) {
    tracker.addObserver(observer)
    tracker.addObserver(theLongPollObserver)
    tracker.initializeState(CombatState.empty)
    theInstance = new Application(tracker)
  }

}

class Application(tracker: Tracker[CombatState]) extends Tracker.Observer[CombatState] {
  private var state: CombatState = CombatState.empty

  tracker.addObserver(this)

  def loadStateFile(fileToSave: File) {
    val f = new CombatSaveFile()
    val state = f.load(new FileInputStream(fileToSave))
    tracker.initializeState(state)
  }

  def saveStateToFile(file: File) {
    val f = new CombatSaveFile()
    f.save(new FileOutputStream(file), state)
  }

  def setCombatComment(combatComment: String) {
    tracker.dispatchAction(SetCombatComment(combatComment), null)
  }

  def stateUpdated(newState: CombatState) {
    state = newState
  }

  def getState = state

}