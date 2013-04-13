/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.web.servlet

import org.scalatra.test.specs2.MutableScalatraSpec
import vcc.dnd4e.web.services.StateViewService
import vcc.dnd4e.tracker.common.CombatState
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PlayerViewServletTest extends MutableScalatraSpec {
  args(sequential = true)

  addServlet(classOf[PlayerViewServlet], "/player-view")

  "GET on player view" should {
    "return current status on immediate call" in {
      mockService(CombatState.empty, None)
      get("/player-view?now=true") {
        status must_== 200
        body must_== expectedState
      }
    }

    "return nothing on long poll with none" in {
      mockService(CombatState.empty, None)
      get("/player-view") {
        status must_== 200
        body must_== "{}"
      }
    }

    "return formatted state when long poll has Some state" in {
      mockService(CombatState.empty, Some(CombatState.empty))
      get("/player-view") {
        status must_== 200
        body must_== expectedState
      }
    }
  }

  private def expectedState: String = {
    "{ \"state\": []}"
  }

  private def mockService(state: CombatState, longPoolState: Option[CombatState]) {
    val pvs = new StateViewService {
      def currentState(): CombatState = state

      def stateAfterChange(timeoutInMillis: Int): Option[CombatState] = longPoolState
    }
    StateViewService.setInstance(pvs)
  }
}