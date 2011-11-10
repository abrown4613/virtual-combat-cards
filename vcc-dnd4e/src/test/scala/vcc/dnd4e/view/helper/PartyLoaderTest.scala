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
package vcc.dnd4e.view.helper

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.domain.compendium.{MonsterEntity, CompendiumRepository}
import vcc.dnd4e.model.{CombatantEntityBuilder, PartyMember}
import vcc.dnd4e.tracker.common.{CombatantID, CombatantRosterDefinition}

class PartyLoaderTest extends SpecificationWithJUnit with Mockito {

  private val entId1 = EntityID.fromName("test:1")
  private val entId2 = EntityID.fromName("test:2")
  private val entId3 = EntityID.fromName("test:3")


  trait alpha extends Scope {
    val peer = mock[PartyLoader.ViewPeer]
    val loader = new PartyLoader(null, peer)
    val partyMembers = List(mock[PartyMember])
  }

  "PartyLoader.validatePartyLoadAndWarn" should {

    "show failed load result" in new alpha {
      loader.validatePartyLoadAndWarn((Nil, false)) must_== Nil
      there was one(peer).showError("Invalid Party File", "File is not a valid Party File. Check vcc.log for details.")
    }

    "show empty file on load" in new alpha {
      loader.validatePartyLoadAndWarn((Nil, true)) must_== Nil
      there was one(peer).showError("Empty Party File", "The party files contains no entries.")
    }

    "pass through on good load" in new alpha {
      loader.validatePartyLoadAndWarn((partyMembers, true)) must_== partyMembers
      there was no(peer).confirm(any, any)
      there was no(peer).showError(any, any)
    }

    "pass partially loaded file if confirmed" in new alpha {
      peer.confirm("Invalid Entries in Party File", "Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?") returns true
      loader.validatePartyLoadAndWarn((partyMembers, false)) must_== partyMembers
      there was one(peer).confirm("Invalid Entries in Party File", "Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?")
    }

    "block load of partially load file on decline" in new alpha {
      peer.confirm("Invalid Entries in Party File", "Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?") returns false
      loader.validatePartyLoadAndWarn((partyMembers, false)) must_== Nil
      there was one(peer).confirm("Invalid Entries in Party File", "Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?")
    }
  }

  trait mocked extends Scope {
    val peer = mock[PartyLoader.ViewPeer]
    val es = mock[CompendiumRepository]
    val members = Seq(
      PartyMember(null, null, entId3),
      PartyMember(CombatantID("A"), "Alice", entId1),
      PartyMember(null, "goblin", entId2),
      PartyMember(null, null, entId2))
    val loader = new PartyLoader(es, peer)
    val monster1 = makeMonster(entId1)
    val monster2 = makeMonster(entId2)
    es.load(entId1, true) returns monster1
    es.load(entId2, true) returns monster2
    es.load(entId3, true) returns null
    val ce1 = CombatantEntityBuilder.fromCompendiumCombatantEntity(monster1)
    val ce2 = CombatantEntityBuilder.fromCompendiumCombatantEntity(monster2)
  }

  "PartyLoader.resolveEntries" should {
    "cleanly load found entries" in new mocked {
      loader.resolveEntries(members.tail) must_== List(
        CombatantRosterDefinition(CombatantID("A"), "Alice", ce1),
        CombatantRosterDefinition(null, "goblin", ce2),
        CombatantRosterDefinition(null, null, ce2))
      there was one(es).load(entId1, true)
      there was one(es).load(entId2, true)
      //there was one(es).load(entId3,true)
    }

    "returned resolved entities on confirm accept missing" in new mocked {
      peer.confirm("Missing Combatants",
        "Not all combatants in the party where found in the compendium.\nDo you wish to load the ones that were found?") returns true
      loader.resolveEntries(members) must_== List(
        CombatantRosterDefinition(CombatantID("A"), "Alice", ce1),
        CombatantRosterDefinition(null, "goblin", ce2),
        CombatantRosterDefinition(null, null, ce2))
      there was one(es).load(entId1, true)
      there was one(es).load(entId2, true)
      there was one(es).load(entId3, true)
    }

    "returned Nil on failed confirm" in new mocked {
      peer.confirm("Missing Combatants",
        "Not all combatants in the party where found in the compendium.\nDo you wish to load the ones that were found?") returns false
      loader.resolveEntries(members) must_== Nil
      there was one(es).load(entId1, true)
      there was one(es).load(entId2, true)
      there was one(es).load(entId3, true)
    }
  }

  private def makeMonster(eid: EntityID) = {
    val monsterEntity = new MonsterEntity(entId1)
    monsterEntity.loadFromMap(Map(
      "base:name" -> ("Some " + eid.asStorageString),
      "stat:hp" -> "40",
      "base:level" -> "4",
      "stat:initiative" -> "2",
      "text:statblock" -> "foo"
    ))
    monsterEntity
  }
}
