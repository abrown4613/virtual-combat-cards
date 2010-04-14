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
import vcc.infra.test.TransactionalSpecification
import vcc.dnd4e.domain.tracker.common.CombatMetaDataChange

@RunWith(classOf[JUnitSuiteRunner])
class CombatMetaDataTest extends JUnit4(CombatMetaDataSpec)

object CombatMetaDataSpec extends Specification with TransactionalSpecification {
  val fiat = "Fiat lux!"
  val aMeta = new CombatMetaData(fiat)

  "aCombatMetaData constructor" should {

    "start with intial text" in {
      aMeta.comment must_== fiat
    }
    "start not in battle" in {
      aMeta.inCombat must beFalse

    }
  }

  "aCombatMetaData under transaction" should {

    "start combat" in {
      withTransaction {
        trans =>
          aMeta.startCombat()(trans)
      } afterCommit {
        changes =>
          aMeta.inCombat must beTrue
          changes must contain(CombatMetaDataChange(true, fiat))
      } afterUndo {
        changes =>
          aMeta.inCombat must beFalse
          changes must contain(CombatMetaDataChange(false, fiat))
      } afterRedoAsInCommit ()
    }
    "end a started battle" in {
      runAndCommit(trans => aMeta.startCombat()(trans))

      withTransaction {
        trans =>
          aMeta.endCombat()(trans)
      } afterCommit {
        changes =>
          aMeta.inCombat must beFalse
          changes must contain(CombatMetaDataChange(false, fiat))
      } afterUndo {
        changes =>
          aMeta.inCombat must beTrue
          changes must contain(CombatMetaDataChange(true, fiat))
      } afterRedoAsInCommit ()
    }

    "alter comment transactional" in {
      withTransaction {
        trans =>
          aMeta.comment_=("Und er war licht!")(trans)
      } afterCommit {
        changes =>
          aMeta.comment must_== "Und er war licht!"
          changes must contain(CombatMetaDataChange(false, "Und er war licht!"))
      } afterUndo {
        changes =>
          aMeta.comment must_== fiat
          changes must contain(CombatMetaDataChange(false, fiat))
      } afterRedoAsInCommit ()
    }
  }
}