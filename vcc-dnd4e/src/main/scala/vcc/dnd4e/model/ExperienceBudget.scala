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

import annotation.tailrec

object ExperienceBudget {
  private val incrementList = List(25, 50, 100, 200, 400, 950, 2000)

  private def nextHeadAndTailFromInfiniteIncrements(increments: List[Int]): (Int, List[Int]) = {
    if (increments.tail.isEmpty)
      (increments.head, List(increments.head * 2))
    else
      (increments.head, increments.tail)
  }

  private def levelMark(level: Int): Int = {
    @tailrec
    def levelMark(runningTotal: Int, level: Int, increments: List[Int]): Int = {
      val (head, tail) = nextHeadAndTailFromInfiniteIncrements(increments)
      if (level < 4) {
        runningTotal + head * level
      } else {
        levelMark(runningTotal + head * 4, level - 4, tail)
      }
    }
    levelMark(100, level - 1, incrementList)
  }

  def levelFromExperience(experiencePoints: Int, numberOfPlayerCharacters: Int): Int = {
    @tailrec
    def levelFromXP(x: Int, l: Int, il: List[Int]): Int = {
      val (head, tail) = nextHeadAndTailFromInfiniteIncrements(il)
      if (x > 4 * head) levelFromXP(x - 4 * head, l + 4, tail)
      else l + (x / head)
    }

    def adjustLevelForLowerBound(experiencePerPC: Int, level: Int): Int = {
      if (experiencePerPC < levelMark(level))
        level - 1
      else
        level
    }

    val experiencePerPC = experiencePoints / math.max(1, numberOfPlayerCharacters)
    val level = levelFromXP(experiencePerPC - 100, 1, incrementList)

    adjustLevelForLowerBound(experiencePerPC, level)
  }
}