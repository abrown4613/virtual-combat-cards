/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.web.util

import vcc.dnd4e.tracker.common.{UnifiedSequenceTable, UnifiedCombatant, CombatState}
import util.parsing.json.JSONObject

class StateFormatter {
  def format(state: CombatState): String = {
    val builder = new UnifiedSequenceTable.Builder
    val unifiedState = builder.build(state)
    if (unifiedState.elements.isEmpty)
      "[]"
    else
      "[\n" + unifiedState.elements.map(formatCombatant).mkString(",\n") + "]"
  }

  private def formatCombatant(comb: UnifiedCombatant): String = {
    val ms: Map[String, Any] = Seq(
      makeField("id", if (comb.isInOrder) comb.orderId.toLabelString else comb.combId.id),
      makeField("name", comb.name),
      makeField("status", comb.health.formattedStatus),
      makeOptionField("health", if (comb.isCharacter) Some(comb.health.formattedHitPoints) else None)
    ).flatMap(x => x).toMap
    JSONObject(ms).toString()
  }

  private def makeField(key: String, value: String): Option[(String, Any)] = {
    Some((key, value))
  }

  private def makeOptionField(key: String, value: Option[String]): Option[(String, Any)] = {
    value.map(value => (key, value))
  }
}