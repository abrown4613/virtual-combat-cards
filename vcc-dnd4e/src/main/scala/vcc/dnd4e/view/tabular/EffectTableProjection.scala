/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.tabular

import vcc.util.swing.TableModelRowProjection
import vcc.dnd4e.tracker.common.{Effect}
import vcc.dnd4e.domain.tracker.common.Command.UpdateEffectCondition
import vcc.dnd4e.view.PanelDirector

class EffectTableProjection(director: PanelDirector) extends TableModelRowProjection[Effect] {
  val columns: List[(String, java.lang.Class[_])] = List(
    ("Src", classOf[String]),
    ("End", classOf[String]),
    ("Description", classOf[String]))

  def apply(col: Int, entry: Effect): java.lang.Object = {
    col match {
      case 0 => entry.source.id
      case 1 => entry.duration.shortDescription
      case 2 => entry.condition.description
    }
  }

  val setter: PartialFunction[(Int, Effect, Any), Unit] = {
    case (2, Effect(eid, _, Effect.Condition.Generic(x, good), _), newValue) =>
      director requestAction UpdateEffectCondition(eid, Effect.Condition.Generic(newValue.asInstanceOf[String], good))
  }
}
