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
package vcc.dnd4e.tracker

import common._
import vcc.scalaz.Lens

trait LensFactory[S]

trait StateLensFactory extends LensFactory[CombatState] {
  val rosterLens: Lens[CombatState, Roster[Combatant]]
  val orderLens: Lens[CombatState, InitiativeOrder]
  protected val combatantHealthLens: Lens[Combatant, HealthTracker]
  protected val combatantEffectLens: Lens[Combatant, EffectList]
  protected val combatantCommentLens: Lens[Combatant, String]
  protected val combatantMapLens: Lens[Roster[Combatant], Map[CombatantID, Combatant]]
  protected val initiativeTrackerMapLens: Lens[InitiativeOrder, Map[InitiativeOrderID, InitiativeTracker]]

  def combatantHealth(cid: CombatantID): Lens[CombatState, HealthTracker] = {
    rosterLens compose combatantMapLens.at(cid) compose combatantHealthLens
  }

  def combatantComment(cid: CombatantID): Lens[CombatState, String] = {
    rosterLens compose combatantMapLens.at(cid) compose combatantCommentLens
  }

  def combatantEffectList(cid: CombatantID): Lens[CombatState, EffectList] = {
    rosterLens compose combatantMapLens.at(cid) compose combatantEffectLens
  }

  def combatant(cid: CombatantID): Lens[CombatState, Combatant] = {
    rosterLens compose combatantMapLens.at(cid)
  }

  def initiativeTrackerLens(ioi: InitiativeOrderID) = {
    orderLens compose initiativeTrackerMapLens.at(ioi)
  }
}

object StateLensFactory extends StateLensFactory {
  protected val combatantHealthLens = Lens[Combatant, HealthTracker](
    c => c.health,
    (c, h) => c.copy(health = h)
  )

  protected val combatantMapLens: Lens[Roster[Combatant], Map[CombatantID, Combatant]] = Lens(
    r => r.entries,
    (r, ne) => r.copy(entries = ne)
  )

  val rosterLens: Lens[CombatState, Roster[Combatant]] = Lens(
    s => s.roster,
    (s, r) => s.copy(roster = r)
  )

  val orderLens: Lens[CombatState, InitiativeOrder] = Lens(
    s => s.order,
    (s, o) => s.copy(order = o)
  )

  protected val combatantCommentLens = Lens[Combatant, String](
    c => c.comment,
    (c, newComment) => c.copy(comment = newComment)
  )

  protected val combatantEffectLens = Lens[Combatant, EffectList](
    c => c.effects,
    (c, el) => c.copy(effects = el)
  )

  protected val initiativeTrackerMapLens: Lens[InitiativeOrder, Map[InitiativeOrderID, InitiativeTracker]] = Lens(
    o => o.tracker,
    (o, ts) => o.copy(tracker = ts)
  )
}
