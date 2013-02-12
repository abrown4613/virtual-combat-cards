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
package vcc.dnd4e.web.servlet

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import vcc.dnd4e.web.services.StateViewService
import java.io.PrintWriter
import vcc.dnd4e.tracker.common.{UnifiedCombatant, UnifiedSequenceTable, CombatState}

class PlayerViewServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    val newState = if (req.getParameterValues("now") == null)
      StateViewService.getInstance.stateAfterChange(15000)
    else
      Some(StateViewService.getInstance.currentState())

    resp.setCharacterEncoding("UTF-8")

    newState match {
      case Some(state) =>
        val writer = resp.getWriter
        generateResponse(writer, state)
      case None =>
        resp.getWriter.append("{}")
    }
  }

  def generateResponse(writer: PrintWriter, state: CombatState) {
    val builder = new UnifiedSequenceTable.Builder
    val unifiedState = builder.build(state)
    writer.print( """{ "state": [""" + "\n")
    writer.append(unifiedState.elements.map(formatCombatant).mkString(",\n"))
    writer.print("]}")
  }

  def formatCombatant(comb: UnifiedCombatant): String = {
    """ { "id":"%s", "name":"%s", "health": "%s", "status": "%s"} """.format(
      if (comb.isInOrder) comb.orderId.toLabelString else comb.combId.id,
      comb.name, comb.health.formattedHitPoints,
      comb.health.formattedStatus)
  }
}