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

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.CombatState
import java.io.{InputStream, ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import vcc.dnd4e.tracker.event.SetCombatCommentEvent

class CombatSaveFileTest extends SpecificationWithJUnit {
  def is = "CombatSaveFile".title ^
    "save empty state" ! saveEmptyState ^
    "save state with comment" ! saveStateWithComment ^
    end

  def saveEmptyState = {
    val combatState = CombatState.empty
    val loadedCombatState: CombatState = storeAndLoadToMemory(combatState)
    combatState must_== loadedCombatState
  }

  private def saveStateWithComment = {
    val combatState = CombatState.empty.transitionWith(List(SetCombatCommentEvent(Some("memorable"))))
    val loadedCombatState: CombatState = storeAndLoadToMemory(combatState)
    combatState must_== loadedCombatState
  }

  private def storeAndLoadToMemory(combatState: CombatState): CombatState = {
    loadFromByteArray(storeToByteArray(combatState))
  }

  private def storeToByteArray(combatState: CombatState): Array[Byte] = {
    val s: CombatSaveFile = new CombatSaveFile();
    val os = new ByteArrayOutputStream();
    s.save(os, combatState)
    os.toByteArray
  }

  private def loadFromByteArray(inputBytes: Array[Byte]): CombatState = {
    val is: InputStream = new ByteArrayInputStream(inputBytes)
    val loadedCombatState: CombatState = new CombatSaveFile().load(is)
    loadedCombatState
  }
}