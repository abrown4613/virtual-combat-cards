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
package vcc.tracker.helper

import vcc.dnd4e.tracker.ruling.RulingAcceptance
import org.specs2.specification.Fragments
import vcc.tracker.Ruling

class FlexRulingTest extends RulingAcceptance[State]("FlexRuling") {

  protected val rulingWithAnswer: Ruling[State, _, _] = FlexRuling("what", Some(List(AlterCommand(1))))
  protected val rulingWithoutAnswer: Ruling[State, _, _] = FlexRuling("what", None)
  protected val userPromptMessage: String = "what [11]"
  protected val state: State = State(11)

  def buildCases: Fragments =
    "make a list of commands pass as argument" ! {
       FlexRuling("what", Some(Nil)).generateCommands(state) must_== Nil
    } ^
      "make a list of commands pass as argument" ! {
        FlexRuling("what", Some(List(AlterCommand(1)))).generateCommands(state) must_== List(AlterCommand(1))
      }
    endp
}
