//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.domain.compendium.CombatantEntity
import vcc.dnd4e.model.CombatantType
import vcc.dnd4e.domain.compendium.Compendium
import org.w3c.dom.Document
import vcc.util.swing.XHTMLPane

/**
 * This SERVICE class is used to get StatBlock for a given combatant based
 * on the EntityID. It will generate a mini-stat block if the combatant does
 * not have a StatBlock or parse an return a valid DOM Document if it does.
 */
object CombatantStatBlockCache {

  private var cache = Map.empty[EntityID,Document]

  def generateMiniBlock(ent:CombatantEntity):String = {
    (<html>
    	<head><link rel="stylesheet" type="text/css" href="dndi.css" /></head>
    	<body>
    		<div id="detail">
    		<h1 class={if(ent.combatantType == CombatantType.Character) "player" else "monster" }>{ent.name.value}<br /><span class="type"></span></h1>
    		<p class="flavor"><b>Initiative</b > {ent.initiative.value} <br/>
    		<b>AC</b> {ent.ac.value} ; <b>Fortitude</b> {ent.fortitude.value}, <b>Reflex</b> {ent.reflex.value}, <b>Will</b> {ent.will.value}<br/></p>
    		</div>
    	</body>
    </html>).toString
  }
  
  def getStatBlockDocumentForCombatant(eid:EntityID,statBlock:String):Document = {
    if(!cache.isDefinedAt(eid)) {
        val doc = XHTMLPane.parsePanelDocument(statBlock)
        if(doc != null) {
          cache = cache + (eid -> doc)
          doc
        } else {
            XHTMLPane.errorDocument
        }
    } else 
    	cache(eid)
  }
}
