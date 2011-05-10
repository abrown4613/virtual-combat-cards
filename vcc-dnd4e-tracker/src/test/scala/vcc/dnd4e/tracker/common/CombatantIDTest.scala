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

import org.specs2.{SpecificationWithJUnit}

class CombatantIDTest extends SpecificationWithJUnit {
  def is =
    "a CombatantID should" ^
      "String backed id should" ^
      "ignore white space" ! ignoreWhiteSpace(" a ") ^
      "be a well defined ID " ^ isWellBehavedCombatantID(CombatantID("A1")) ^
      endp ^
      "Integer like string id should" ^
      "ignore white space" ! ignoreWhiteSpace(" 1 ") ^
      "be a well defined ID " ^ isWellBehavedCombatantID(CombatantID("12")) ^
      "provide itself as a number" ^ isNumericBacked(CombatantID("14")) ^
      endp ^
      "a list of Ids should" ^ listBuildingFragment ^
      end

  def listBuildingFragment =
    "be filtered out Int base ids" ! exampleMakeList(List(CombatantID("10"), CombatantID("A"), CombatantID("11")), List(10, 11)) ^
      "and this other " ! exampleMakeList(List(CombatantID("21"), CombatantID("A1")), List(21)) ^
      "filter to Nil if no numbers" ! exampleMakeList(List(CombatantID("B"), CombatantID("A1")), Nil)

  case class definedCombatant(comb: CombatantID) {
    def hasToString = comb.toString must_== "CombatantID(" + comb.id + ")"

    def matchesCombatantWithSameId = comb must_== CombatantID(comb.id)

    def pointsToSameObject = comb must beEqualTo(CombatantID(comb.id))

    def hasSameHashCode = comb.hashCode must_== CombatantID(comb.id).hashCode
  }

  def exampleMakeList(ids: List[CombatantID], mustMatch: List[Int]) =
    ids.flatMap(x => x.asNumber) must_== mustMatch

  def ignoreWhiteSpace(s: String) = CombatantID(s).id must_== s.trim

  def isNumericBacked(comb: CombatantID) =
    "be a IntCombatantID" ! {
      comb.asNumber.isDefined must beTrue
    } ^
      "represent it's text" ! {
        comb.asNumber.get.toString must_== comb.id
      }

  def isWellBehavedCombatantID(comb: CombatantID) = {
    "provides proper to string" ! definedCombatant(comb).hasToString ^
      "matches a copy of self" ! definedCombatant(comb).matchesCombatantWithSameId ^
      "points to same fly-weight" ! definedCombatant(comb).pointsToSameObject ^
      "have same hashcode" ! definedCombatant(comb).hasSameHashCode
  }
}