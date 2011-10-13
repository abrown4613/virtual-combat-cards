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
package vcc.dnd4e.tracker.ruling

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.{CombatState, SampleStateData}
import vcc.dnd4e.tracker.transition.{MoveUpCommand, StartRoundCommand}
import vcc.tracker.IllegalDecisionException

class NextUpRulingTest extends SpecificationWithJUnit with SampleStateData {

  private val combatState = CombatState.empty
  private val r = NextUpRuling(EligibleNext(ioA0, List(io1_0, ioB0)), None)

  def is =
    "NextUpRuling" ^
      "has standard message" ! e1 ^
      "generate StartRount if main selected" ! e2 ^
      "generate MoveUp if some other is selected" ! e3 ^
      "not allow answers " ! e4 ^
      end

  private def e1 = {
    r.question.userPrompt(combatState) must_== "Select which combatant should act next"
  }

  private def e2 = {
    r.withDecision(ioA0).generateCommands(combatState) must_== List(StartRoundCommand(ioA0))
  }

  private def e3 = {
    (r.withDecision(io1_0).generateCommands(combatState) must_== List(MoveUpCommand(io1_0))) and
      (r.withDecision(ioB0).generateCommands(combatState) must_== List(MoveUpCommand(ioB0)))
  }

  private def e4 = {
    r.withDecision(io2_0) must throwA(new IllegalDecisionException(io2_0 + " is not eligible to act"))
  }
}