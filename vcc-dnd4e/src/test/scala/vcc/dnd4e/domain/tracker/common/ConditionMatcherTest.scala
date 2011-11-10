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
package vcc.dnd4e.domain.tracker.common

import org.specs2.SpecificationWithJUnit

class ConditionMatcherTest extends SpecificationWithJUnit {

  import ConditionMatcher._

  def is =
    "splitFirstCondition" ^ e1 ^ endp ^
      "Regeneration matcher" ^ e2 ^ endp ^
      "Ongoing matcher" ^ e3 ^ endp ^
      end

  private def e1 = {
    val cases = List(
      ("slowed and weak", List("slowed", "weak")),
      ("slowed aNd Weak", List("slowed", "Weak")),
      ("slowed and weak and     dead", List("slowed", "weak", "dead")),
      ("slowed", List("slowed")),
      ("slowed-> weak", List("slowed")),
      ("slowed -> weak", List("slowed"))
    )
    for ((str, ret) <- cases) yield {
      ("split " + str + " to " + ret) ! {
        splitFirstCondition(str) must_== ret
      }
    }
  }

  private def e2 = {
    val cases = List(
      ("regenerate 2", ("regenerate 2", 2)),
      ("regen 2", ("regen 2", 2)),
      ("regeneration 2", ("regeneration 2", 2)),
      ("Regeneration 2", ("Regeneration 2", 2)),
      ("+2 def -> regeneration 2", null),
      ("regeneration 2->+2 def", ("regeneration 2", 2)),
      ("regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("regenerate 5 while bloodied and +2 defense", ("regenerate 5 while bloodied", 5)),
      ("+2 defense and regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("+2 defense regenerate 5 while bloodied", ("regenerate 5 while bloodied", 5)),
      ("slowed and weakened", null),
      ("ongoing 4 fire", null)
    )
    for ((str, ret) <- cases) yield {
      ("match " + str) ! {
        FirstRegenerate.unapply(str) must_== Option(ret)
      }
    }
  }

  private def e3 = {
    val cases = List(
      ("ongoing 4 fire", ("ongoing 4 fire", 4)),
      ("ongoing 4 fire and immobilized", ("ongoing 4 fire", 4)),
      ("slowed and ongoing 4", ("ongoing 4", 4)),
      ("slowed ongoing 4", ("ongoing 4", 4)),
      ("ONgoing 10 (1d20) and spooked", ("ONgoing 10 (1d20)", 10)),
      ("regenerate 2", null),
      ("regen 2", null),
      ("ongoing 4 fire -> ongoing 10 cold", ("ongoing 4 fire", 4)),
      ("Ongooing 124 fire & thunder and insustancia", null)
    )
    for ((str, ret) <- cases) yield {
      "match " + str ! {
        FirstOngoing.unapply(str) must_== Option(ret)
      }
    }
  }
}