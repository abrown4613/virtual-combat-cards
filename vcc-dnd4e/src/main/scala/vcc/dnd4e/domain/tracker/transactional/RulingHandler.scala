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
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.domain.tracker.common.Command.InternalInitiativeAction
import vcc.controller.message.TransactionalAction
import vcc.controller.{RulingDecisionHandler, Ruling, PendingRuling}
import vcc.dnd4e.domain.tracker.common._

object RulingSearchService {

  type R = Ruling with RulingDecisionHandler[List[TransactionalAction]]

  private def endRoundMatcher(who: CombatantID): PartialFunction[Effect, R] = {
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEnd) => (SaveEffectRuling.fromEffect(effect))
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEndSpecial) => (SaveEffectSpecialRuling.fromEffect(effect))
  }

  def searchEndRound(context: CombatState, who: CombatantID): List[PendingRuling[List[TransactionalAction]]] = {
    val whoMatcher = endRoundMatcher(who)
    context.allEffects.flatMap(whoMatcher.lift(_)).map(new PendingRuling(_)).toList
  }

  def searchStartRound(context: CombatState, who: CombatantID): List[PendingRuling[List[TransactionalAction]]] = Nil

}

trait RulingHandler {
  this: AbstractCombatController =>

  import InitiativeTracker.action._

  addRulingSearch{
    case InternalInitiativeAction(who, EndRound) => RulingSearchService.searchEndRound(context, who.combId)
    case InternalInitiativeAction(who, StartRound) => RulingSearchService.searchStartRound(context, who.combId)
  }
}