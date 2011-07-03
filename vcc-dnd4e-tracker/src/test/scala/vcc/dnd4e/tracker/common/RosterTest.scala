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
//$Id$
package vcc.dnd4e.tracker.common

import org.specs2.SpecificationWithJUnit
import org.specs2.specification.Fragments
import org.specs2.mock.Mockito

object RosterTestHelper {

  case class SampleEntry(key: CombatantID, alias: String, entity: CombatantEntity)

  class Factory extends RosterCombatantFactory[SampleEntry] {

    def createCombatant(definition: CombatantRosterDefinition): SampleEntry = SampleEntry(definition.cid, definition.alias, definition.entity)

    def replaceDefinition(combatant: SampleEntry, newDefinition: CombatantRosterDefinition): SampleEntry = SampleEntry(newDefinition.cid, newDefinition.alias, newDefinition.entity)
  }

}

class RosterTest extends SpecificationWithJUnit {

  import RosterTestHelper._

  type T = SampleEntry

  val combEnt = CombatantEntity(null, "Mage", CharacterHealthDefinition(40), 4, CombatantType.Character, null)
  val combEnt2 = CombatantEntity(null, "Fighter", CharacterHealthDefinition(50), 4, CombatantType.Character, null)
  val combMonster = CombatantEntity(null, "Bad Guy", CharacterHealthDefinition(50), 4, CombatantType.Monster, null)
  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val comb1 = CombatantID("1")
  val comb2 = CombatantID("2")
  val comb3 = CombatantID("3")

  val factory = new Factory()
  val fullRoster = new Roster[T](factory, Map(
    combA -> SampleEntry(combA, null, combEnt),
    combB -> SampleEntry(combB, "el palo", combEnt2),
    comb1 -> SampleEntry(comb1, null, combMonster),
    comb3 -> SampleEntry(comb3, null, combMonster)
  ))

  val emptyRoster = new Roster[T](factory, Map())

  val mRoster = mocked(fullRoster)

  def is: Fragments =
    "when handling addComabatant should" ^
      "  provide an ID if not supplied" ! using(emptyRoster).updatedAllOnAdd(None, comb1, null, combMonster) ^
      "  accept provided ID" ! using(emptyRoster).updatedAllOnAdd(Some(combA), combA, null, combEnt) ^
      "  update an entry calling proper create method when new ID" ! mRoster.calledCreateDefinition(comb2, "the bad", combMonster) ^
      "  update an entry calling proper replace method when ID exists" ! mRoster.calledReplaceDefinition(combA, "the bad", combMonster) ^
      "  update an all fields on replace method when ID exists" ! using(fullRoster).updatedAllOnAdd(Some(combA), combA, null, combMonster) ^
      "  provide first free id in range" ! using(fullRoster).updatedAllOnAdd(None, comb2, "abc", combMonster) ^
      endp ^
      "a fully loaded Roster should" ^
      "  call predicate test on all elements" ! mRoster.callPredicateNTimeOnClear(x => x.key.asNumber.isDefined) ^
      "  remove elements that do not match test " ! using(fullRoster).filterElements(x => x.key.asNumber.isDefined, List(combA, combB)) ^
      "  provide a list of combatantID" ! using(fullRoster).provideListOfElement(List(combA, combB, comb1, comb3)) ^
      endp ^
      "as a view should" ^
      "  indicated that ID is present" ! bePresent(fullRoster, combA) ^
      "  indicated that ID is present" ! bePresent(fullRoster, comb1) ^
      "  indicated that ID is not present" ! notBePresent(fullRoster, comb2) ^
      "  return present entry with ID" ! (fullRoster.combatant(combA) must_== SampleEntry(combA, null, combEnt)) ^
      "  throw exception on getting a not present ID" ! (fullRoster.combatant(comb2) must throwA[NoSuchElementException]) ^
      end

  def bePresent(roster: Roster[SampleEntry], comb: CombatantID) = roster.isDefinedAt(comb) must beTrue

  def notBePresent(roster: Roster[SampleEntry], comb: CombatantID) = roster.isDefinedAt(comb) must beFalse


  case class mocked(roster: Roster[T]) extends Mockito {
    val mFactory = spy(roster.factory)
    val mRoster = roster.copy(factory = mFactory)

    def calledReplaceDefinition(combId: CombatantID, alias: String, definition: CombatantEntity) = {
      mRoster.addCombatant(Some(combId), alias, definition)
      there was one(mFactory).replaceDefinition(roster.combatant(combId), CombatantRosterDefinition(combId, alias, definition))
    }

    def calledCreateDefinition(expectedId: CombatantID, alias: String, definition: CombatantEntity) = {
      mRoster.addCombatant(None, alias, definition)
      there was one(mFactory).createCombatant(CombatantRosterDefinition(expectedId, alias, definition))
    }

    def callPredicateNTimeOnClear(pred: T => Boolean) = {
      class FWrap extends Function1[T, Boolean] {
        def apply(v1: T): Boolean = pred(v1)
      }
      val spyPred = spy(new FWrap)
      roster.clear(spyPred)
      there was atLeast(roster.entries.size)(spyPred).apply(any[T])
    }
  }

  case class using(roster: Roster[T]) {
    def updatedAllOnAdd(definedId: Option[CombatantID], expectedId: CombatantID, alias: String, definition: CombatantEntity) = {
      val r = roster.addCombatant(definedId, alias, definition)
      r.entries must beDefinedAt(expectedId) and havePair(expectedId -> SampleEntry(expectedId, alias, definition))
    }

    def filterElements(pred: T => Boolean, expectedList: List[CombatantID]) = {
      val r = roster.clear(pred)
      r.allCombatantIDs must_== expectedList
    }

    def provideListOfElement(expectedList: List[CombatantID]) = {
      roster.allCombatantIDs must contain(expectedList.map(lazyfy(_)): _*)
    }
  }

}