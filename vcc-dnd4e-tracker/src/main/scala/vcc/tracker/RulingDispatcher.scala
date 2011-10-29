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
package vcc.tracker

trait RulingPeer[S] {
  def provideDecisionForRuling(state: S, rulings: List[Ruling[S, _, _]]): List[Ruling[S, _, _]]
}

trait RulingLocationService[S] {
  def rulingsFromStateWithCommand(state: S, command: StateCommand[S]): List[Ruling[S, _, _]]
}

class RulingDispatcher[S](peer: RulingPeer[S], rls: RulingLocationService[S]) {
  def dispatch(state: S, command: StateCommand[S]): List[StateCommand[S]] = {
    val rulings: List[Ruling[S, _, _]] = rls.rulingsFromStateWithCommand(state, command)
    val decisions = peer.provideDecisionForRuling(state, rulings)

    if (!rulings.zip(decisions).forall(p => p._1.isRulingSameSubject(p._2) && p._2.hasDecision))
      throw new Exception("Fail")
    decisions.flatMap(d => d.generateCommands(state)) ::: List(command)
  }
}

class StateCommandDispatcher[S](ruling: RulingDispatcher[S]) {
  def dispatch(state: S, command: StateCommand[S], builder: TransitionBuilder[S, _]): S = {
    val commands = ruling.dispatch(state, command)
    commands.foldLeft(state)((s, c) => {
      val events = c.generateTransitions(s)
      builder.addEvents(events)
      events.foldLeft(s)((s2, e) => e.transition(s2))
    })
  }
}