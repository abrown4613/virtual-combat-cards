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
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import vcc.dnd4e.tracker.event.{AddCombatantEvent, SetCombatCommentEvent}
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.tracker.common._

class CombatSaveFileTest extends SpecificationWithJUnit {
  def is = "CombatSaveFile".title ^
    "save empty state" ! saveEmptyState ^
    "save state with comment" ! saveStateWithComment ^
    "save state with multiple combatants" ! saveStateWithCombatant ^
    end

  private def saveEmptyState = {
    val combatState = CombatState.empty
    val loadedCombatState: CombatState = storeAndLoadToMemory(combatState)
    loadedCombatState must_== combatState
  }

  private def saveStateWithComment = {
    val combatState = CombatState.empty.transitionWith(List(SetCombatCommentEvent(Some("memorable"))))
    val loadedCombatState: CombatState = storeAndLoadToMemory(combatState)
    loadedCombatState must_== combatState
  }

  private def saveStateWithCombatant = {
    val entity1 = CombatantEntity(EntityID.generateRandom().asStorageString, "Fighter", CharacterHealthDefinition(30), 5, "<html><body>block</body></html>")
    val entity2 = CombatantEntity(EntityID.generateRandom().asStorageString, "Goblin", MonsterHealthDefinition(25), 3, "<html><body>monster</body></html>")
    val entity3 = CombatantEntity(EntityID.generateRandom().asStorageString, "Goblin-mini", MinionHealthDefinition, 1, "<html><body>minion</body></html>")
    val combatState = CombatState.empty.transitionWith(List(
      AddCombatantEvent(Some(CombatantID("A")), "alias", entity1),
      AddCombatantEvent(None, null, entity2),
      AddCombatantEvent(None, "boss", entity3)))
    val loadedCombatState = storeAndLoadToMemory(combatState)
    loadedCombatState must_== combatState
  }

  private def storeAndLoadToMemory(combatState: CombatState): CombatState = {
    loadFromByteArray(storeToByteArray(combatState))
  }

  private def storeToByteArray(combatState: CombatState): Array[Byte] = {
    val s = new CombatSaveFile();
    val os = new ByteArrayOutputStream();
    s.save(os, combatState)
    os.toByteArray
  }

  private def loadFromByteArray(inputBytes: Array[Byte]): CombatState = {
    val is = new ByteArrayInputStream(inputBytes)
    val loadedCombatState: CombatState = new CombatSaveFile().load(is)
    loadedCombatState
  }
}