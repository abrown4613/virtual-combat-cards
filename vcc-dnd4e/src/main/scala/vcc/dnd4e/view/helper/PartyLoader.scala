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

import vcc.dnd4e.model.{PartyFile,PartyMember}
import vcc.dnd4e.domain.compendium.{CombatantEntity=>CompendiumCombatantEntity}
import vcc.dnd4e.controller.request.{AddCombatants,CombatantDefinition}
import vcc.dnd4e.domain.compendium.{Compendium,CompendiumRepository}
import vcc.model.Registry
import vcc.infra.datastore.naming._
import vcc.dnd4e.model.{CombatantEntityID,CombatantEntity,CombatantRepository}

import scala.swing.{Dialog,Component}

object PartyLoader {
  
  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  def loadToBattle(director:PanelDirector,owner:Component,file:java.io.File) {
	val res = PartyFile.loadFromFile(file)
	logger.debug("Loaded party: "+res)
	val lst = validatePartyLoadAndWarn(owner,res)
	if(!lst.isEmpty) loadToBattle(director,owner,lst)
  }
  
  def validatePartyLoadAndWarn(owner:Component, pair:(Seq[PartyMember],Boolean)):Seq[PartyMember] = {
	pair match {
	  case (Nil,false) =>
	    Dialog.showMessage(owner,"File is not a valid Party File. Check vcc.log for details.","Invalid Party File",Dialog.Message.Error,null)
        Nil                
	  case (Nil,true) =>
	    Dialog.showMessage(owner,"The party files contains no entries.","Empty Party File",Dialog.Message.Error,null)
        Nil
	  case (lst,false) =>
	    if(Dialog.showConfirmation(owner,"Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?","Invalid Entries in Party File",Dialog.Options.YesNo) == Dialog.Result.Yes)
           lst
        else
           Nil
	  case (lst,true) =>
	    lst
	}
    
  }
  
  def loadToBattle(director:PanelDirector,owner:Component, members:Seq[PartyMember]) {
    val esid = Registry.get[DataStoreURI]("Compendium").get
	val es = Registry.get[CompendiumRepository](esid).get
	val idMap = scala.collection.mutable.Map.empty[EntityID,CombatantEntityID]
 
    val cds:Seq[CombatantDefinition] = (for(pm <- members) yield {
      if(!idMap.isDefinedAt(pm.eid)) {
        val cent = es.load(pm.eid,true).asInstanceOf[CompendiumCombatantEntity]
        if(cent == null) {
          logger.error("PartyLoader: failed to load entity {}",pm.eid)
          null.asInstanceOf[CombatantDefinition]
        } else {
          val ent = CombatantEntity.fromCompendiumCombatantEntity(cent)
    	  val ceid = CombatantRepository.registerEntity(ent)
    	  if(ent != null) idMap += (pm.eid -> ceid)
        }
      }
      if(idMap.isDefinedAt(pm.eid)) {
        val ceid = idMap(pm.eid)
        CombatantDefinition(pm.id,pm.alias,idMap(pm.eid))
      } else 
        null
    })
      
    val filtered = cds.filter(x=> x!= null)
    if(filtered.length == cds.length || 
        Dialog.showConfirmation(owner,"Not all combatants in the party where found in the compendium.\nDo you wish to load the ones that were found?","Missing Combatants",Dialog.Options.YesNo) == Dialog.Result.Yes
    ) {
      director requestAction AddCombatants(filtered)
    }
  }

}
