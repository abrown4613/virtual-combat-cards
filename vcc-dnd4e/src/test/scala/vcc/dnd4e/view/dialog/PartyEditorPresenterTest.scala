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
package vcc.dnd4e.view.dialog

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.view.dialog.PartyEditorView.PartyTableEntry
import vcc.dnd4e.compendium.Compendium
import vcc.dnd4e.model.PartyBuilder.EntryDefinition
import vcc.infra.datastore.naming.EntityID
import org.specs2.specification.Scope
import vcc.dnd4e.model.PartyBuilder
import vcc.dnd4e.tracker.common.CombatantID

class PartyEditorPresenterTest extends SpecificationWithJUnit with Mockito {

  private val mockCompendium = new MockedCompendium()
  mockCompendium.initialize()

  trait scope extends Scope {
    val pp = new PartyEditorPresenter()
    val mockView = mock[PartyEditorView.UIElement]
    pp.setView(mockView)
  }

  trait spyScope extends Scope {
    val builderSpy = spy(new PartyBuilder())
    val pp = new PartyEditorPresenter(builderSpy)
    val mockView = mock[PartyEditorView.UIElement]
    pp.setView(mockView)
  }

  "PartyEditorPresenter" should {
    "update table when we add" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      there was one(mockView).setExperienceMessage("100 XP")
      there was one(mockView).setPartyTableContent(List(entryFromEntityID(entity1)))
    }

    "add a trap" in new scope {
      val entity1 = getTrap(2)
      pp.addEntry(entity1)

      there was one(mockView).setExperienceMessage("300 XP")
      there was one(mockView).setPartyTableContent(List(entryFromEntityID(entity1)))
    }

    "add a character" in new scope {
      val entity1 = getCharacter(4)
      pp.addEntry(entity1)

      there was one(mockView).setExperienceMessage("0 XP")
      there was one(mockView).setPartyTableContent(List(entryFromEntityID(entity1)))
    }

    "add one of each" in new scope {
      val entity1 = getMonster(0)
      val entity2 = getTrap(2)
      val entity3 = getCharacter(4)

      pp.addEntry(entity1)
      pp.addEntry(entity2)
      pp.addEntry(entity3)

      there was atLeastOne(mockView).setExperienceMessage("400 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(
        entryFromEntityID(entity1), entryFromEntityID(entity2), entryFromEntityID(entity3)))
    }

    "update quantity of a combatant" in new scope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.changeQuantity(0, 2)

      there was atLeastOne(mockView).setExperienceMessage("600 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(
        PartyTableEntry(None, 1, None, getEntityDefinition(entity1).name, getEntityDefinition(entity1).experience),
        PartyTableEntry(None, 1, None, getEntityDefinition(entity1).name, getEntityDefinition(entity1).experience)))
    }

    "update alias of combatant" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeAlias(0, "some alias")
      there was atLeastOne(mockView).setExperienceMessage("100 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(entryFromEntityID(entity1, alias = Some("some alias"))))
    }

    "update alias of combatant to blank" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeAlias(0, "")
      there was atLeastOne(mockView).setExperienceMessage("100 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(entryFromEntityID(entity1, alias = None)))
    }

    "update CombatantID of combatant" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeCombatantId(0, "ida")
      there was atLeastOne(mockView).setExperienceMessage("100 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(entryFromEntityID(entity1, cid = Some(CombatantID("IDA")))))
    }

    "update CombatantID of combatant then blank" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeCombatantId(0, "ida")
      pp.changeCombatantId(0, "")
      there was atLeastOne(mockView).setExperienceMessage("100 XP")
      there was atLeastOne(mockView).setPartyTableContent(List(entryFromEntityID(entity1, cid = Some(CombatantID("IDA"))))) then
        two(mockView).setPartyTableContent(List(entryFromEntityID(entity1)))
    }

    "collapse similar entries" in new spyScope {
      val entity1 = getMonster(2)
      pp.collapseEntries()
      there was one(builderSpy).collapseSimilar()
      pp.addEntry(entity1)
      pp.addEntry(entity1)
      there was atLeastOne(mockView).setPartyTableContent(List(
        entryFromEntityID(entity1, 2)))
    }

    "expand similar entries" in new spyScope {
      val entity1 = getMonster(2)
      pp.collapseEntries()
      there was one(builderSpy).collapseSimilar()
      pp.addEntry(entity1)
      pp.addEntry(entity1)
      pp.expandEntries()
      there was one(builderSpy).expandSimilar()
      there was atLeastOne(mockView).setPartyTableContent(List(
        entryFromEntityID(entity1), entryFromEntityID(entity1)
      ))
    }

    "check for well formed combatant ID" in new spyScope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.isValidCombatantID(0, "a") must beTrue
      there was one(builderSpy).wouldViolateIdUniqueness(CombatantID("A"), 0)
    }

    "check for well malformed combatant ID" in new spyScope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.isValidCombatantID(0, "-") must beFalse
      there was no(builderSpy).wouldViolateIdUniqueness(any, any)
    }

    "check ID uniqueness violation" in new spyScope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.addEntry(entity1)

      pp.changeCombatantId(0, "a")
      pp.isValidCombatantID(1, "A") must beFalse
      there was one(builderSpy).wouldViolateIdUniqueness(CombatantID("A"), 1)
    }
    
    "clear all and update view" in new spyScope {
      pp.clearAll()
      there was one(builderSpy).clear()
      there was one(mockView).setPartyTableContent(Nil)
      there was one(mockView).setExperienceMessage("0 XP")
    }
  }

  private def getMonster(index: Int): EntityID = {
    Compendium.activeRepository.getMonsterSummaries()(index).eid
  }

  private def getTrap(index: Int): EntityID = {
    Compendium.activeRepository.getTrapSummaries()(index).eid
  }

  private def getCharacter(index: Int): EntityID = {
    Compendium.activeRepository.getCharacterSummaries()(index).eid
  }

  private def getEntityDefinition(entityId: EntityID): EntryDefinition = {
    PartyEditorPresenter.resolveEntity(entityId).copy(entityId = null)
  }

  private def entryFromEntityID(entityId: EntityID, quantity: Int = 1, alias: Option[String] = None, cid: Option[CombatantID] = None): PartyEditorView.PartyTableEntry = {
    PartyTableEntry(cid, quantity, alias, getEntityDefinition(entityId).name, getEntityDefinition(entityId).experience)
  }

}