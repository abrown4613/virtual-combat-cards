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
package vcc.domain.dndi

import vcc.model.Registry
import vcc.dnd4e.model.{Compendium,MonsterEntity}
import vcc.model.datastore.{EntityFactory,EntityID,DataStoreURI,Field}

/**
 * This service is used to get Monster form the DNDI Capture model into 
 * the MonsterEntity model.
 */
object MonsterImportService {
  
  val logger = org.slf4j.LoggerFactory.getLogger("domain")
  
  def importMonster(dndiMonster: Monster) {
     if(dndiMonster==null) return  
     val es = Compendium.activeRepository
     val eid = DataStoreURI.asEntityID("vcc-ent:dndi:monster:"+dndiMonster.id)
     val monster = EntityFactory.createInstance(Compendium.monsterClassID,eid).asInstanceOf[MonsterEntity]
     logger.debug("Load D&DI Monster: {}",dndiMonster)
     val fieldMap = Map[String,Field[_]](
       "NAME"->monster.name,
       "HP"->monster.hp,
       "AC"->monster.ac,
       "REFLEX"->monster.reflex,
       "WILL"->monster.will,
       "FORTITUDE"->monster.fortitude,
       "ROLE"->monster.role,
       "XP"->monster.xp,
       "INITIATIVE"->monster.initiative,
       "LEVEL"->monster.level
     )
     for((key,field)<-fieldMap) {
       val v = dndiMonster(key) 
       try {
    	   if(v.isDefined )field.fromStorageString(v.get)
    	   else logger.warn("Mapping failed between {} and {}",key,field.datumKey)
       } catch {
         case e => 
           logger.error("Exception will processing "+key,e)
       }
     }
     val xml = MonsterStatBlockBuilder.generate(dndiMonster)
     monster.statblock.value = xml.toString
     logger.debug("Imported MonsterEntity in XML: {}",monster.toXML)
     es.store(monster)
     logger.info("Imported MonsterEntity: {}",monster.eid)
  }
}
