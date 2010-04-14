/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
import vcc.model.IDGenerator
import vcc.dnd4e.model.common.CombatantType
import vcc.dnd4e.model.CombatantEntity
import vcc.infra.test.{TransactionalSpecification, TransactionChangeLogger}
import vcc.controller.transaction.{ChangeNotification, Transaction}
import vcc.dnd4e.domain.tracker.common.{CombatantAspect, RosterChange, CombatantID}

@RunWith(classOf[JUnitSuiteRunner])
class CombatantRosterTest extends JUnit4(CombatantRosterSpec)

object CombatantRosterSpec extends Specification with TransactionalSpecification with Mockito {
  var aRoster: CombatantRoster = null
  var mockIDGenerator: IDGenerator = null

  val combEnt = CombatantEntity(null, "Bond", vcc.dnd4e.model.common.CharacterHealthDefinition(40, 10, 6), 4, CombatantType.Character, null)

  val blankRoster = beforeContext {
    aRoster = new CombatantRoster()
  }

  val blankRosterWithMock = beforeContext {
    mockIDGenerator = mock[IDGenerator]
    aRoster = new CombatantRoster(mockIDGenerator)
  }

  "a CombatantRoster interacting with IDGenerator" ->- (blankRosterWithMock) should {
    "ask for and ID if non is provided" in {
      mockIDGenerator.first() returns Symbol("1")

      aRoster.addCombatant(null, "alias", combEnt)(new Transaction())

      aRoster.combatant(CombatantID("1")) must notBeNull
      there was one(mockIDGenerator).first()
    }

    "check IDGenerator for provided ID but done remove if it's not in generator" in {
      mockIDGenerator.contains(Symbol("A")) returns false
      aRoster.addCombatant(CombatantID(Symbol("A").name), "alias", combEnt)(new Transaction())

      there was one(mockIDGenerator).contains(Symbol("A"))
      there was no(mockIDGenerator).removeFromPool(Symbol("A"))
    }

    "remove ID from generator if provided and is contained in generator" in {
      mockIDGenerator.contains(Symbol("10")) returns true
      aRoster.addCombatant(CombatantID(Symbol("10").name), "alias", combEnt)(new Transaction())

      there was one(mockIDGenerator).contains(Symbol("10")) then
              one(mockIDGenerator).removeFromPool(Symbol("10"))
    }

    "return ID to pool when an add is undone" in {
      val trans = new Transaction()
      val changeLog = new TransactionChangeLogger()
      mockIDGenerator.first() returns Symbol("20")
      mockIDGenerator.contains(Symbol("20")) returns true
      mockIDGenerator.contains(Symbol("A")) returns false

      aRoster.addCombatant(null, "alias", combEnt)(trans)
      aRoster.addCombatant(CombatantID("A"), "alias", combEnt)(trans)
      trans.commit(changeLog)
      trans.undo(changeLog)

      there was one(mockIDGenerator).returnToPool(Symbol("20"))
      there was no(mockIDGenerator).returnToPool(Symbol("A")) // This was not generated don't return...
    }

    "remove ID to pool when an add is redone" in {
      val trans = new Transaction()
      val changeLog = new TransactionChangeLogger()
      mockIDGenerator.first() returns Symbol("20")
      mockIDGenerator.contains(Symbol("20")) returns true
      mockIDGenerator.contains(Symbol("A")) returns false

      aRoster.addCombatant(null, "alias", combEnt)(trans)
      aRoster.addCombatant(CombatantID("A"), "alias", combEnt)(trans)
      trans.commit(changeLog)
      trans.undo(changeLog)
      trans.redo(changeLog)

      there was one(mockIDGenerator).removeFromPool(Symbol("20"))
      there was no(mockIDGenerator).removeFromPool(Symbol("A")) // This was not generated don't return...
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
          rc.keys.next must notBeNull
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
          changes must exist(x => x match {
            case RosterChange(cmap) => !cmap.isEmpty
            case _ => false
          })
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
      (nc -- rc).length must_== 1
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