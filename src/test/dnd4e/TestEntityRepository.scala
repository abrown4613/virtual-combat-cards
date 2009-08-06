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
package test.dnd4e

import vcc.dnd4e.model.{CombatantEntity,CombatantType,MonsterEntity,CharacterEntity}
import vcc.model.datastore._

object TestEntityRepository extends EntityStore {
  
  
  val entIKnow= Map[String,CombatantEntity](
    "vcc-ent:dndi:monster:665" -> new MonsterEntity(DataStoreURI.asEntityID("vcc-ent:dndi:monster:665")) {
      hp.value = 25
      initiative.value = 7
      name.value = "Goblin Warrior"
      ac.value = 17
      fortitude.value = 13
      reflex.value = 15
      will.value = 12
    }, 
    "vcc-ent:pc:kantrex:3"-> new CharacterEntity(DataStoreURI.asEntityID("vcc-ent:pc:kantrex:3")) {
      hp.value = 44
      initiative.value = 5
      name.value = "Kantrex"
      ac.value = 23
      fortitude.value = 18
      reflex.value = 17
      will.value = 18
    },
    "vcc-ent:pc:magelan:3" -> new CharacterEntity(DataStoreURI.asEntityID("vcc-ent:pc:magelan:3")) {
      hp.value = 22
      initiative.value = 4
      name.value = "Magelan"
      ac.value = 17
      fortitude.value = 17
      reflex.value = 19
      will.value = 20
    },
    "vcc-ent:pc:aramil:3" -> new CharacterEntity(DataStoreURI.asEntityID("vcc-ent:pc:aramil:3")) {
      hp.value = 22
      initiative.value = 4
      name.value = "Aramil the Forgetfull"
      ac.value = 17
      fortitude.value = 17
      reflex.value = 19
      will.value = 20
    }
  )
  
  def fetchEntityID(eid:String):EntityID = {
    try {
      if(entIKnow.isDefinedAt(eid))
    	EntityID(new java.net.URI(eid))
      else 
    	null
    } catch {
      case _ => null
    }
  }

  
  def load(eid:EntityID):Entity = {
    if(entIKnow.isDefinedAt(eid.uri.toString)) entIKnow(eid.uri.toString).asInstanceOf[Entity]
    else null.asInstanceOf[Entity]
  }
  
  protected def getEntitySummaryMap(eid:EntityID):Map[DatumKey,String] = Map.empty[DatumKey,String]
  
  def delete(eid:EntityID) { }
  
  def store(entity: Entity) {
    //DO nothing
  }

  def getEntityClassID(eid:EntityID): Option[EntityClassID] = Some(EntityClassID(new java.net.URI("vcc:combatant")))
  
  def enumerate(classId:EntityClassID):Set[EntityID] = {
    println("Ents"+entIKnow.keys.map(x=>DataStoreURI.asEntityID(x)).toList)
    Set(entIKnow.keys.map(DataStoreURI.asEntityID(_)).toList : _*)
  }
  
}
