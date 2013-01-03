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
package vcc.dnd4e.model

import org.specs2.mutable.SpecificationWithJUnit
import vcc.infra.datastore.naming.EntityID
import org.specs2.specification.Scope
import vcc.dnd4e.tracker.common.CombatantID
import org.specs2.mock.Mockito

class PartyBuilderTest extends SpecificationWithJUnit {
  private val entity1 = EntityID.generateRandom()
  private val entity2 = EntityID.generateRandom()

  private val entry1 = PartyBuilder.EntryDefinition(entity1, "some name", 100)
  private val entry2 = PartyBuilder.EntryDefinition(entity2, "other name", 50)
  private val combA = CombatantID("A")
  private val combB = CombatantID("B")

  trait context extends Scope {
    protected val partyBuilder = new PartyBuilder

    protected def checkRow(row: Int, definition: PartyBuilder.EntryDefinition, quantity: Int, alias: Option[String] = None) {
      partyBuilder.getExperience(row) must_== definition.experience
      partyBuilder.getName(row) must_== definition.name
      partyBuilder.getQuantity(row) must_== quantity
      partyBuilder.getId(row) must_== None
      partyBuilder.getAlias(row) must_== alias
    }

    protected def checkRowWithId(row: Int, definition: PartyBuilder.EntryDefinition, id: CombatantID) {
      partyBuilder.getExperience(row) must_== definition.experience
      partyBuilder.getName(row) must_== definition.name
      partyBuilder.getQuantity(row) must_== 1
      partyBuilder.getId(row) must_== Some(id)
    }

    protected def checkTotals(size: Int, totalExperience: Int) {
      partyBuilder.numberOfRows must_== size
      partyBuilder.encounterExperience must_== totalExperience
    }

    protected def addParticipants(entries: PartyBuilder.EntryDefinition*) {
      for (entry <- entries)
        partyBuilder.addEncounterParticipant(entry)
    }

    protected def addNTimes(time: Int, entry: PartyBuilder.EntryDefinition) {
      var i = time
      while (i > 0) {
        partyBuilder.addEncounterParticipant(entry)
        i -= 1;
      }
    }

    protected def checkAllRows(entries: PartyBuilder.EntryDefinition*) {
      entries.zipWithIndex.foreach(p => checkRow(p._2, p._1, 1))
    }

  }

  trait loadedCollapsed extends context {
    addParticipants(entry1, entry2, entry1, entry2, entry1)
    partyBuilder.collapseSimilar()
  }

  trait contextWithResolver extends context with Mockito {
    val resolver = mock[PartyBuilder.EntityResolver]
    resolver.resolveEntity(entity1) returns entry1
    resolver.resolveEntity(entity2) returns entry2
  }

  "PartyBuilder" should {
    "add a single combatant" in new context {
      addParticipants(entry1)
      checkTotals(1, 100)
      checkRow(0, entry1, 1)
      partyBuilder.getId(0) must_== None
      partyBuilder.getAlias(0) must_== None
    }

    "add a two combatant" in new context {
      addParticipants(entry1, entry2)
      checkTotals(2, 150)
      checkRow(0, entry1, 1)
      checkRow(1, entry2, 1)
    }

    "add combatant and set id" in new context {
      addParticipants(entry1, entry2, entry1)
      partyBuilder.wouldViolateIdUniqueness(combA, 0) must beFalse
      partyBuilder.setId(0, Some(combA))
      partyBuilder.getId(0) must_== Some(combA)
      partyBuilder.wouldViolateIdUniqueness(combA, 0) must beFalse
      partyBuilder.wouldViolateIdUniqueness(combA, 1) must beTrue
      partyBuilder.wouldViolateIdUniqueness(combA, 2) must beTrue
    }

    "throw exception on repeated entries" in new context {
      addParticipants(entry1, entry1)
      partyBuilder.setId(0, Some(combA))
      partyBuilder.setId(1, Some(combA)) must throwA(new PartyBuilder.CombatantIdMustBeUniqueException(combA))
    }

    "allow redefining the same id" in new context {
      addParticipants(entry1, entry1)
      partyBuilder.setId(0, Some(combA))
      partyBuilder.setId(0, Some(combA))
      partyBuilder.getId(0) must_== Some(combA)
    }

    "add combatant and set alias" in new context {
      addParticipants(entry1)
      partyBuilder.setAlias(0, Some("alias"))
      partyBuilder.getAlias(0) must_== Some("alias")
    }

    "add several combatant and collapse similar" in new context {
      addParticipants(entry1, entry2, entry1, entry2, entry1)
      checkTotals(5, 400)
      partyBuilder.collapseSimilar()
      checkTotals(2, 400)
      checkRow(0, entry1, 3)
      checkRow(1, entry2, 2)
    }

    "preserve ids when collapsing" in new context {
      addNTimes(5, entry1)
      partyBuilder.setId(0, Some(combA))
      partyBuilder.setId(4, Some(combB))
      partyBuilder.collapseSimilar()
      checkRowWithId(0, entry1, combA)
      checkRow(1, entry1, 3)
      checkRowWithId(2, entry1, combB)
    }

    "preserve alias when collapsing" in new context {
      addNTimes(5, entry1)
      partyBuilder.setAlias(0, Some("alias1"))
      partyBuilder.setAlias(3, Some("alias1"))
      partyBuilder.setAlias(1, Some("alias2"))
      partyBuilder.setAlias(4, Some("alias2"))
      partyBuilder.collapseSimilar()

      checkRow(0, entry1, 2, Some("alias1"))
      checkRow(1, entry1, 2, Some("alias2"))
      checkRow(2, entry1, 1)
    }

    "collapse then add several combatant" in new context {
      partyBuilder.collapseSimilar()
      addParticipants(entry1, entry2, entry1, entry2, entry1)
      checkTotals(2, 400)
      checkRow(0, entry1, 3)
      checkRow(1, entry2, 2)
      partyBuilder.isCollapsed must beTrue
    }

    "setting quantity updates entries" in new context {
      addParticipants(entry1)
      partyBuilder.setQuantity(0, 3)
      checkTotals(3, 300)
    }

    "remove row when setting quantity to zero" in new context {
      addParticipants(entry1, entry2)
      partyBuilder.setQuantity(0, 0)
      checkTotals(1, 50)
      checkRow(0, entry2, 1)
    }

    "empty builder on a clear" in new context {
      addParticipants(entry1, entry2, entry1, entry2)
      partyBuilder.clear()
      checkTotals(0, 0)
    }
  }

  "PartyBuilder in collapsed mode" should {

    "expanding entries after collapsing" in new loadedCollapsed {
      partyBuilder.expandSimilar()
      checkTotals(5, 400)
      checkAllRows(entry1, entry1, entry1, entry2, entry2)
      partyBuilder.isCollapsed must beFalse
    }

    "setting alias on collapsed entry breaks row" in new loadedCollapsed {
      partyBuilder.setId(0, Some(combA))
      checkTotals(3, 400)
      checkRowWithId(0, entry1, combA)
      checkRow(1, entry1, 2)
      partyBuilder.getId(1) must_== None
      checkRow(2, entry2, 2)
    }

    "setting id on collapsed entry breaks row into two" in new loadedCollapsed {
      partyBuilder.setId(1, Some(combA))
      checkTotals(3, 400)
      checkRow(0, entry1, 3)
      checkRowWithId(1, entry2, combA)
      partyBuilder.getId(2) must_== None
      checkRow(2, entry2, 1)
    }

    "setting id then clearing should collapse row" in new loadedCollapsed {
      partyBuilder.setId(1, Some(combA))
      partyBuilder.setId(1, None)
      checkRow(1, entry2, 2)
    }

    "collapse entries when changing alias" in new context {
      addNTimes(3, entry1)
      partyBuilder.setAlias(0, Some("alias1"))
      partyBuilder.setAlias(2, Some("alias1"))
      partyBuilder.collapseSimilar()
      partyBuilder.setAlias(1, Some("alias1"))

      checkRow(0, entry1, 3, Some("alias1"))
    }

    "remove row when setting quantity to zero" in new loadedCollapsed {
      partyBuilder.setQuantity(1, 0)
      checkTotals(1, 300)
      checkRow(0, entry1, 3)
    }
  }

  "PartyBuilder loading file" should {
    "load single entry" in new contextWithResolver {
      val party = List(PartyMember(null, null, entity1))
      partyBuilder.loadFromParty(resolver, party)
      there was one(resolver).resolveEntity(entity1);
      checkTotals(1, 100)
      checkRow(0, entry1, 1)
    }
    "load single entry with alias and id" in new contextWithResolver {
      val party = List(PartyMember(combA, "alias", entity1))
      partyBuilder.loadFromParty(resolver, party)
      checkRowWithId(0, entry1, combA)
      partyBuilder.getAlias(0) must_== Some("alias")
    }

    "load a complex party" in new contextWithResolver {
      val party = List(
        PartyMember(combA, null, entity1),
        PartyMember(combB, "alias", entity2),
        PartyMember(null, "mini", entity1),
        PartyMember(null, "mini", entity1),
        PartyMember(null, null, entity1))

      partyBuilder.loadFromParty(resolver, party)
      partyBuilder.collapseSimilar()
      checkTotals(4, 450)
      checkRowWithId(0, entry1, combA)
      checkRowWithId(1, entry2, combB)
      checkRow(2, entry1, 2, Some("mini"))
      checkRow(3, entry1, 1)
    }

    "load single entry with alias and id" in new contextWithResolver {
      val party = List(PartyMember(combA, null, entity1), PartyMember(combA, null, entity2))
      partyBuilder.loadFromParty(resolver, party)
      checkRowWithId(0, entry1, combA)
      checkRow(1, entry2, 1)
    }
  }

  "PartyBuilder generating party" should {
    "save single entry party" in new context {
      addParticipants(entry1)
      partyBuilder.generateParty() must_== List(PartyMember(null, null, entity1))
    }

    "save single entry party with ids" in new context {
      addParticipants(entry1)
      partyBuilder.setAlias(0, Option("alias"))
      partyBuilder.setId(0, Some(combA))
      partyBuilder.generateParty() must_== List(PartyMember(combA, "alias", entity1))
    }

    "save compressed to party" in new loadedCollapsed {
      partyBuilder.setAlias(0, Some("alias"))
      partyBuilder.setId(0, Some(combA))
      partyBuilder.setId(2, Some(combB))
      
      val party = partyBuilder.generateParty()
      party.size must_== 5
      party must_== List(
        PartyMember(combA,"alias",entity1),
        PartyMember(null,"alias",entity1),
        PartyMember(null,"alias",entity1),
        PartyMember(combB,null,entity2),
        PartyMember(null,null,entity2))
    }
  }
}