/**
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
package vcc.dnd4e.domain.tracker.transactional

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.infra.test.{TransactionalSpecification, TransactionChangeLogger}
import vcc.controller.transaction.{ChangeNotification, Transaction}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._

@RunWith(classOf[JUnitSuiteRunner])
class CombatantRosterTest extends JUnit4(CombatantRosterSpec)

object CombatantRosterSpec extends Specification with TransactionalSpecification with Mockito {
  var aRoster: CombatantRoster = null

  val combA = CombatantID("A")
  val combB = CombatantID("1")

  val combEnt = CombatantEntity(null, "Bond", CharacterHealthDefinition(40), 4, CombatantType.Character, null)
  val combEnt2 = CombatantEntity(null, "Bond", CharacterHealthDefinition(50), 4, CombatantType.Character, null)
  val combMonster = CombatantEntity(null, "Bond", CharacterHealthDefinition(50), 4, CombatantType.Monster, null)

  val blankRoster = beforeContext {
    aRoster = new CombatantRoster()
  }

  val blankRosterWithMock = beforeContext {
    aRoster = new CombatantRoster()
  }

  shareVariables() // TO allow transactional to run better

  "a CombatantRoster interacting with IDGenerator" ->- (blankRosterWithMock) should {
    "ask for and ID if non is provided" in {
      aRoster.addCombatant(null, "alias", combEnt)(new Transaction())

      aRoster.combatant(CombatantID("1")) must notBeNull
    }
  }

  "a blank Roster" ->- (blankRoster) should {
    "addCombatant with undefined ID" in {
      withTransaction {
        atrans =>
          aRoster.addCombatant(null, "alias", combEnt)(atrans)
      } afterCommit {
        changes =>
          val rc = getChangeRosterMap(changes)
          rc must notBeEmpty
          rc.size must_== 1
          rc.keys.head must notBeNull
      } afterUndo {
        changes =>
          changes must contain(RosterChange(Map()))
      } afterRedoAsInCommit
    }

    "addCombatant with defined ID" in {
      val combId = CombatantID("A")
      withTransaction {
        atrans =>
          aRoster.addCombatant(combId, "alias", combEnt)(atrans)
      } afterCommit {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr must beDefinedAt(combId)
          aRoster.combatant(combId) must notBeNull
      } afterUndo {
        changes =>
          changes must contain(RosterChange(Map()))
          aRoster.combatant(combId) must throwA[NoSuchElementException]
      } afterRedoAsInCommit
    }

    "generate a new unique id when an add, then undo, then redo is issued" in {
      var trans = new Transaction()
      val changeLog = new TransactionChangeLogger()
      aRoster.addCombatant(null, "alias", combEnt)(trans)
      trans.commit(changeLog)
      val rc = getChangeRosterMap(changeLog.changes).keys.toList
      rc.length must_== 1
      trans.undo(changeLog)
      trans.redo(changeLog)
      //Now add a new guy, must have ID of 2
      trans = new Transaction()
      aRoster.addCombatant(null, "alias2", combEnt)(trans)
      trans.commit(changeLog)
      val nc = getChangeRosterMap(changeLog.changes).keys.toList
      (nc filterNot (rc contains)).length must_== 1
    }
  }

  "A loaded Roster" ->- (beforeContext {
    val combId = CombatantID("A")
    aRoster = new CombatantRoster()
    runAndCommit {
      trans =>
        aRoster.addCombatant(combId, "alias", combEnt)(trans)
        aRoster.combatant(combId).health_=(aRoster.combatant(combId).health.applyDamage(10))(trans) // Apply damage
    }
  }) should {
    "update a Combatant, preserving HP, if a new definition is given for the same ID" in {
      /*
         The semantic is to update the fields and not change then entire combatant
       */
      val combId = CombatantID("A")
      withTransaction {
        trans =>
          aRoster.addCombatant(combId, "alias 2", combEnt2)(trans)
      } afterCommit {
        changes =>
          getChangeRosterMap(changes) must beEmpty
          changes must contain(CombatantChange(combId, HealthTracker(40, 0, 0, combEnt2.healthDef)))
          changes must contain(CombatantChange(combId, CombatantRosterDefinition(combId, "alias 2", combEnt2)))
      } afterUndo {
        changes =>
          getChangeRosterMap(changes) must beEmpty
          changes must contain(CombatantChange(combId, HealthTracker(30, 0, 0, combEnt.healthDef)))
          changes must contain(CombatantChange(combId, CombatantRosterDefinition(combId, "alias", combEnt)))
      } afterRedoAsInCommit ()
    }
  }

  "a fully loaded Roster" ->- (beforeContext {
    aRoster = new CombatantRoster()
    runAndCommit {
      trans =>
        aRoster.addCombatant(combA, null, combEnt)(trans)
        aRoster.addCombatant(null, null, combMonster)(trans)
    }
  }) should {

    "remove NP combatant on a clear npc" in {
      withTransaction {
        trans => aRoster.clear(false)(trans)
      } afterCommit {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr must beDefinedAt(combA)
          cr mustNot beDefinedAt(combB)
      } afterUndo {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr must beDefinedAt(combA)
          cr must beDefinedAt(combB)
      } afterRedo {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr must beDefinedAt(combA)
          cr mustNot beDefinedAt(combB)
      }
    }

    "remove NP combatant on a clear all" in {
      withTransaction {
        trans => aRoster.clear(true)(trans)
      } afterCommit {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr mustNot beDefinedAt(combA)
          cr mustNot beDefinedAt(combB)
      } afterUndo {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr must beDefinedAt(combA)
          cr must beDefinedAt(combB)
      } afterRedo {
        changes =>
          val cr = getChangeRosterMap(changes)
          cr mustNot beDefinedAt(combA)
          cr mustNot beDefinedAt(combB)
      }
    }
  }

  def getChangeRosterMap(changes: Seq[ChangeNotification]): Map[CombatantID, Set[CombatantAspect]] = {
    val rc = changes.find(x => x.isInstanceOf[RosterChange])
    rc match {
      case Some(RosterChange(map)) => map
      case _ => Map()
    }
  }
}