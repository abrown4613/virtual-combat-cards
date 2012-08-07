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

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.DataTables

object ExperienceBudgetData extends DataTables {
  val table = "level" | "4 pc" | "5 pc" | "6 pc" |
    1 ! 400 ! 500 ! 600 |
    2 ! 500 ! 625 ! 750 |
    3 ! 600 ! 750 ! 900 |
    4 ! 700 ! 875 ! 1050 |
    5 ! 800 ! 1000 ! 1200 |
    6 ! 1000 ! 1250 ! 1500 |
    7 ! 1200 ! 1500 ! 1800 |
    8 ! 1400 ! 1750 ! 2100 |
    9 ! 1600 ! 2000 ! 2400 |
    10 ! 2000 ! 2500 ! 3000 |
    11 ! 2400 ! 3000 ! 3600 |
    12 ! 2800 ! 3500 ! 4200 |
    13 ! 3200 ! 4000 ! 4800 |
    14 ! 4000 ! 5000 ! 6000 |
    15 ! 4800 ! 6000 ! 7200 |
    16 ! 5600 ! 7000 ! 8400 |
    17 ! 6400 ! 8000 ! 9600 |
    18 ! 8000 ! 10000 ! 12000 |
    19 ! 9600 ! 12000 ! 14400 |
    20 ! 11200 ! 14000 ! 16800 |
    21 ! 12800 ! 16000 ! 19200 |
    22 ! 16600 ! 20750 ! 24900 |
    23 ! 20400 ! 25500 ! 30600 |
    24 ! 24200 ! 30250 ! 36300 |
    25 ! 28000 ! 35000 ! 42000 |
    26 ! 36000 ! 45000 ! 54000 |
    27 ! 44000 ! 55000 ! 66000 |
    28 ! 52000 ! 65000 ! 78000 |
    29 ! 60000 ! 75000 ! 90000 |
    30 ! 76000 ! 95000 ! 114000 |
    31 ! 92000 ! 115000 ! 138000
}

class ExperienceBudgetTest extends SpecificationWithJUnit {
  def is =
    "Calculate level on budget" ! levelOnBudget ^
      "Calculate level for slight below budget" ! slightUnderBudget ^
      "Calculate level for slight over budget" ! slightOverBudget ^
      "Lower bound" ! lowerBound ^
      end


  def levelOnBudget = {
    ExperienceBudgetData.table |> {
      (level, pc4, pc5, pc6) =>
        (ExperienceBudget.levelFromExperience(pc4, 4) must_== level) and
          (ExperienceBudget.levelFromExperience(pc5, 5) must_== level) and
          (ExperienceBudget.levelFromExperience(pc6, 6) must_== level)
    }
  }

  def slightUnderBudget = {
    ExperienceBudgetData.table |> {
      (level, pc4, pc5, pc6) =>
        (ExperienceBudget.levelFromExperience(pc5 - 10, 5) must_== (level - 1))
    }
  }

  def slightOverBudget = {
    ExperienceBudgetData.table |> {
      (level, pc4, pc5, pc6) =>
        (ExperienceBudget.levelFromExperience(pc5 + 10, 5) must_== level)
    }
  }

  def lowerBound = {
    (ExperienceBudget.levelFromExperience(100, 5) must_== 0) and
      (ExperienceBudget.levelFromExperience(400, 5) must_== 0)
  }
}