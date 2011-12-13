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
package vcc.dnd4e.tracker.common

import org.specs2.{SpecificationWithJUnit}

class CombatantIDTest extends SpecificationWithJUnit {

  private val badCases: List[String] = "A-/1+1/A B/a:1/a(/c)/8*".split("/").toList

  def is =
    "a CombatantID should" ^
      "String backed id should" ^
      "ignore white space" ! ignoreWhiteSpaceChangeToUpperCase(" a ") ^
      "be a well defined ID " ^ isWellBehavedCombatantID(CombatantID("A1")) ^
      "be a well defined ID 2" ^ isWellBehavedCombatantID(CombatantID("A_1")) ^
      endp ^
      "Integer like string id should" ^
      "  ignore white space" ! ignoreWhiteSpaceChangeToUpperCase(" 1 ") ^
      "  be a well defined ID " ^ isWellBehavedCombatantID(CombatantID("12")) ^
      "  provide itself as a number" ^ isNumericBacked(CombatantID("14")) ^
      endp ^
      "a list of Ids should" ^ listBuildingFragment ^
      endp ^
      "fail on illegal character" ^ failBadCases(badCases) ^ endp ^
      "provide test for illegal characters" ^ notAllowedIds(badCases) ^ endp ^
      "provide test for legal characters" ^ allowedIds(List("A1", "11", "A_1", "_1", " _1")) ^ endp ^
      end

  private def listBuildingFragment =
    "be filtered out Int base ids" ! exampleMakeList(List(CombatantID("10"), CombatantID("A"), CombatantID("11")), List(10, 11)) ^
      "and this other " ! exampleMakeList(List(CombatantID("21"), CombatantID("A1")), List(21)) ^
      "filter to Nil if no numbers" ! exampleMakeList(List(CombatantID("B"), CombatantID("A1")), Nil)

  case class definedCombatant(comb: CombatantID) {
    def serializeToXMLNotation = CombatantID.fromXMLNotation(comb.toXMLNotation) must_== Some(comb)

    def hasToString = comb.toString must_== "CombatantID(" + comb.id + ")"

    def matchesCombatantWithSameId = comb must_== CombatantID(comb.id)

    def matchesCombatantWithSameIdWithoutCase = comb must_== CombatantID(comb.id.toLowerCase)

    def pointsToSameObject = comb must beEqualTo(CombatantID(comb.id))

    def hasSameHashCode = comb.hashCode must_== CombatantID(comb.id).hashCode
  }

  private def failBadCases(badCases: List[String]) = {
    for (c <- badCases) yield {
      c + " must throw exception" ! {
        CombatantID(c) must throwA[IllegalArgumentException]
      }
    }
  }

  private def notAllowedIds(badCases: List[String]) = {
    for (c <- badCases) yield {
      c + " should be invalid" ! {
        CombatantID.isValidID(c) must beFalse
      }
    }
  }

  private def allowedIds(badCases: List[String]) = {
    for (c <- badCases) yield {
      c + " should be valid" ! {
        CombatantID.isValidID(c) must beTrue
      }
    }
  }

  private def exampleMakeList(ids: List[CombatantID], mustMatch: List[Int]) =
    ids.flatMap(x => x.asNumber) must_== mustMatch

  private def ignoreWhiteSpaceChangeToUpperCase(s: String) = CombatantID(s).id must_== s.trim.toUpperCase

  private def isNumericBacked(comb: CombatantID) =
    "be a IntCombatantID" ! {
      comb.asNumber.isDefined must beTrue
    } ^
      "represent it's text" ! {
        comb.asNumber.get.toString must_== comb.id
      }

  private def isWellBehavedCombatantID(comb: CombatantID) = {
    "provides proper to string" ! definedCombatant(comb).hasToString ^
      "matches a copy of self" ! definedCombatant(comb).matchesCombatantWithSameId ^
      "points to same fly-weight" ! definedCombatant(comb).pointsToSameObject ^
      "fly-weight must be case independent"! definedCombatant(comb).matchesCombatantWithSameIdWithoutCase ^
      "have same hashcode" ! definedCombatant(comb).hasSameHashCode ^
      "serialize/deserialize XML notation" ! definedCombatant(comb).serializeToXMLNotation ^
      endp
  }
}