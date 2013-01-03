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
package vcc.dnd4e.view.helper

import org.w3c.dom.Document
import vcc.util.swing.XHTMLPane

/**
 * This SERVICE class is used to get StatBlock for a given combatant based
 * on the EntityID. It will generate a mini-stat block if the combatant does
 * not have a StatBlock or parse an return a valid DOM Document if it does.
 */
object CombatantStatBlockCache {
  private var cache = Map.empty[String, Document]

  def getStatBlockDocumentForCombatant(eid: String, statBlock: String): Document = {
    if (!cache.isDefinedAt(eid)) {
      val doc = XHTMLPane.parsePanelDocument(statBlock)
      if (doc != null) {
        cache = cache + (eid -> doc)
        doc
      } else {
        XHTMLPane.errorDocument
      }
    } else
      cache(eid)
  }
}
