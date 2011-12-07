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
package vcc.acceptance

import java.lang.String
import org.specs2.execute.StandardResults
import org.junit.Assert
import vcc.tracker.Tracker
import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.dispatcher.CombatStateViewAdapterBuilder
import vcc.dnd4e.domain.tracker.common.CombatStateView
import concurrent.SyncVar
import vcc.dnd4e.application.Application
import java.io.File

class ApplicationDriver extends Tracker.Observer[CombatState] {
  private val state: SyncVar[CombatStateView] = new SyncVar[CombatStateView]()

  Application.initialize(this)

  def saveCombatState(fileToSave: String) = {
    application.saveStateToFile(new File(fileToSave))
    this
  }

  def loadCombatState(fileToLoad: String) = {
    application.loadStateFile(new File(fileToLoad))
    this
  }

  def stateUpdated(newState: CombatState) {
    state.set(CombatStateViewAdapterBuilder.buildView(newState))
  }

  def close() = StandardResults.success

  def getCombatState: CombatStateView = {
    Thread.sleep(200)
    state.get
  }

  def checkCombatantComment(expectedComment: String) = {
    Assert.assertEquals(expectedComment, getCombatState.combatComment)
    this
  }

  def setCombatComment(comment: String): ApplicationDriver = {
    application.setCombatComment(comment)
    this
  }

  private def application = Application.getInstance
}