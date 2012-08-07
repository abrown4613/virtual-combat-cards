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
import vcc.dnd4e.view.PanelDirector
import vcc.dnd4e.tracker.common.Command.AddCombatants
import vcc.dnd4e.tracker.common.{CombatantRosterDefinition, CombatantID}
import vcc.dnd4e.model.{CombatantEntityBuilder, PartyMember, PartyFile, PartyBuilder}
import java.io.{FileInputStream, File}

class PartyEditorPresenterTest extends SpecificationWithJUnit with Mockito {

  private val mockCompendium = new MockedCompendium()
  mockCompendium.initialize()

  trait scope extends Scope {
    val pp = new PartyEditorPresenter()
    val mockView = mock[PartyEditorView.UIElement]
    pp.setView(mockView)

    protected def loadPartyInDirectOrder(list: List[PartyMember]) {
      var idx = 0
      for (l <- list) {
        pp.addEntry(l.eid)
        pp.changeAlias(idx, l.alias)
        pp.changeCombatantId(idx, if (l.id != null) l.id.id else null)
        idx += 1
      }
    }
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

      there was one(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1)))
    }

    "add a trap" in new scope {
      val entity1 = getTrap(2)
      pp.addEntry(entity1)

      there was one(mockView).setPartyTableContent(300, List(entryFromEntityID(entity1)))
    }

    "add a character" in new scope {
      val entity1 = getCharacter(4)
      pp.addEntry(entity1)

      there was one(mockView).setPartyTableContent(0, List(entryFromEntityID(entity1)))
    }

    "add one of each" in new scope {
      val entity1 = getMonster(0)
      val entity2 = getTrap(2)
      val entity3 = getCharacter(4)

      pp.addEntry(entity1)
      pp.addEntry(entity2)
      pp.addEntry(entity3)

      there was atLeastOne(mockView).setPartyTableContent(400, List(
        entryFromEntityID(entity1), entryFromEntityID(entity2), entryFromEntityID(entity3)))
    }

    "update quantity of a combatant" in new scope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.changeQuantity(0, 2)

      there was atLeastOne(mockView).setPartyTableContent(600, List(
        PartyTableEntry(None, 1, None, getEntityDefinition(entity1).name, getEntityDefinition(entity1).experience),
        PartyTableEntry(None, 1, None, getEntityDefinition(entity1).name, getEntityDefinition(entity1).experience)))
    }

    "update alias of combatant" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeAlias(0, "some alias")
      there was atLeastOne(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1, alias = Some("some alias"))))
    }

    "update alias of combatant to blank" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeAlias(0, "")
      there was atLeastOne(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1, alias = None)))
    }

    "update CombatantID of combatant" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeCombatantId(0, "ida")
      there was atLeastOne(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1, cid = Some(CombatantID("IDA")))))
    }

    "update CombatantID of combatant then blank" in new scope {
      val entity1 = getMonster(0)
      pp.addEntry(entity1)

      pp.changeCombatantId(0, "ida")
      pp.changeCombatantId(0, "")
      there was atLeastOne(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1, cid = Some(CombatantID("IDA"))))) then
        two(mockView).setPartyTableContent(100, List(entryFromEntityID(entity1)))
    }

    "collapse similar entries" in new spyScope {
      val entity1 = getMonster(2)
      pp.collapseEntries()
      there was one(builderSpy).collapseSimilar()
      pp.addEntry(entity1)
      pp.addEntry(entity1)
      there was atLeastOne(mockView).setPartyTableContent(600, List(
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
      there was atLeastOne(mockView).setPartyTableContent(600, List(
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

    "accept blank combatant ID" in new spyScope {
      val entity1 = getMonster(2)
      pp.addEntry(entity1)
      pp.isValidCombatantID(0, "") must beTrue
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
      there was one(mockView).setPartyTableContent(0, Nil)
    }

    "load party member" in new scope {
      val list = makePartyList()
      pp.loadPartyMembers(list)
      there was one(mockView).setPartyTableContent(1100, List(
        entryFromEntityID(list(1).eid, cid = Some(CombatantID("A"))),
        entryFromEntityID(list(0).eid, alias = Some("alias"))
      ))
    }

    "load party with repeated entries with compressed view" in new scope {
      val member = PartyMember(null, null, getMonster(0))
      pp.collapseEntries()
      pp.loadPartyMembers(List(member, member, member))
      there was one(mockView).setPartyTableContent(300, List(entryFromEntityID(getMonster(0), quantity = 3)))
    }

    "loading party should clear current list" in new scope {
      val member = PartyMember(null, null, getMonster(0))
      pp.collapseEntries()
      pp.addEntry(getMonster(5))
      pp.loadPartyMembers(List(member, member, member))
      there was one(mockView).setPartyTableContent(300, List(entryFromEntityID(getMonster(0), quantity = 3)))
    }

    "load to battle" in new scope {
      val list = makePartyList()
      val director = mock[PanelDirector]
      loadPartyInDirectOrder(list)
      pp.addPartyToBattle(director)
      val m1 = makeCombatantRosterDefinition(list(1), "Monster 3")
      val m2 = makeCombatantRosterDefinition(list(0), "Monster 7")
      there was one(director).requestAction(AddCombatants(List(m1, m2)))

      def makeCombatantRosterDefinition(member: PartyMember, name: String) = {
        val entity = Compendium.activeRepository.load(member.eid, true)
        CombatantRosterDefinition(member.id, member.alias, CombatantEntityBuilder.fromCompendiumCombatantEntity(entity))
      }
    }

    "save to file" in new scope {
      val file = File.createTempFile("vcc", "peml")
      val list = makePartyList()

      loadPartyInDirectOrder(list)

      pp.saveToFile(file)

      val loaded = PartyFile.loadFromStream(new FileInputStream(file))
      file.delete()
      loaded must_==(List(list(1), list(0)), true)
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

  private def makePartyList() = {
    val member2 = PartyMember(CombatantID("A"), null, getMonster(6))
    val member1 = PartyMember(null, "alias", getMonster(3))
    List(member1, member2)
  }
}