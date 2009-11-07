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
package vcc.dnd4e.model

import scala.xml._

import vcc.infra.datastore.naming._
import vcc.dnd4e.domain.compendium.{Compendium,CompendiumRepository}
import vcc.model.Registry

case class PartyMember(id: Symbol, alias:String, eid: EntityID) {
  def toXML() = <combatant eid={eid.uri.toString} alias={if(alias!=null) alias else null} id={if(id!=null) id.name else null} />
}

/**
 * Load a Party of PEML file.
 */
object PartyLoader {
  private val logger = org.slf4j.LoggerFactory.getLogger("domain")
  import vcc.util.XMLHelper._
  def parseEntry(store: CompendiumRepository, node:Node):List[PartyMember] = {
    val eidUri = nodeSeq2String(node \ "@eid")
    val id = nodeSeq2String(node \ "@id", null)
    val alias = nodeSeq2String(node \ "@alias", null)
    val count = nodeSeq2Int(node \ "@count", 1)
    val eid = EntityID.fromStorageString(eidUri)
    
    val ent=PartyMember(if(id!=null) Symbol(id.toUpperCase) else null ,alias,eid)
    // Unroll count
    (1 to count).map(x => ent).toList
  }
  
  def loadFromXML(store: DataStoreURI, node:Node):List[PartyMember] = {
    var x:List[PartyMember]=Nil
    val repository = Registry.get[CompendiumRepository](store).get
    val ver = { 
      val vnl = node \ "@version"
      if(vnl.isEmpty) None
      else Some(vnl(0).text)
    }
    if(node.label=="party" && ver == Some("1.0")) {
      for(snode <- node.child if(snode.label!="#PCDATA")) {
        try {
          x= x ::: parseEntry(repository,snode)
        }catch {
          case e:Exception=>
            logger.warn("Failed to load node: "+snode,e)
        }
      }
    } else {
      if(ver==None) logger.error("No version found on file, this may be a legacy file")
      else logger.error("Failed to load party, either is not a party")
    }
    x
  }
  
  def loadFromFile(store: DataStoreURI, file:java.io.File):List[PartyMember] = {
    try {
      var node=scala.xml.XML.loadFile(file)
      loadFromXML(store,node)
    } catch {
      case e =>
        logger.error("Failed to load: "+e.getMessage,e)
        Nil
    }
  }
  
  def saveToFile(store:DataStoreURI, file:java.io.File,entries:Seq[PartyMember]) {
    val doc = (<party esid={ (if(store!=null) store.uri.toString else null)} version='1.0'>
      {entries.map(_.toXML)}
    </party>)
    XML.saveFull(file.toString,doc,"UTF-8",true,null)
  }
  
  def loadToBattle(esid:DataStoreURI, members:Seq[PartyMember]) {
    
    import vcc.dnd4e.domain.compendium.{CombatantEntity=>CompendiumCombatantEntity}
    import vcc.dnd4e.controller.request.{AddCombatants,CombatantDefinition}
    
	val tracker = Registry.get[scala.actors.Actor]("tracker").get
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
    }).filter(x=> x!= null)
    tracker ! AddCombatants(cds)
  }
}
