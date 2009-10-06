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
package vcc.dnd4e.domain.compendium

import vcc.infra.datastore.naming._
import vcc.infra.datastore.DataStoreEntity
import vcc.infra.fields._
import vcc.model.Registry
import vcc.dnd4e.model.CombatantType

case class EntityClassID(uri: java.net.URI)

case class EntitySummary(eid:EntityID, classid:EntityClassID)

object CombatantEntityFields {
  object RequiredString extends DefaultStringFieldValidator(Mandatory())
  object RequiredIntGreaterZero extends DefaultIntFieldValidator(Mandatory(),BoundedInteger(1 to 2000000000))
  object RequiredInt extends DefaultIntFieldValidator(Mandatory())
  object AnyInt extends DefaultIntFieldValidator()
  object AnyString extends DefaultStringFieldValidator()
}

abstract class CombatantEntity(val eid:EntityID) extends FieldSet(eid) {
  import CombatantEntityFields._ 
  
  val classID:EntityClassID
  
  def combatantType: CombatantType.Value
  
  val name = new StringField(this,"base:name", RequiredString)
 
  val initiative = new IntField(this,"stat:initiative", RequiredInt)
  val hp = new IntField(this,"stat:hp", RequiredIntGreaterZero)

  val ac = new IntField(this,"stat:ac", AnyInt)
  val fortitude = new IntField(this,"stat:fortitude", AnyInt)
  val reflex = new IntField(this,"stat:reflex", AnyInt)
  val will = new IntField(this,"stat:will", AnyInt) 
  
  val statblock = new StringField(this,"text:statblock", AnyString)
  
  override def asDataStoreEntity:DataStoreEntity = {
    val es = super.asDataStoreEntity
    DataStoreEntity(es.eid, es.data + ("classid"->classID.uri.toString))
  }
}

class MonsterEntity(eid:EntityID) extends CombatantEntity(eid) {
  import CombatantEntityFields._
  val classID = Compendium.monsterClassID
  
  def combatantType = if(hp==1) CombatantType.Minion else CombatantType.Monster
  
  val role = new StringField(this,"base:role", RequiredString)
  val level = new IntField(this,"base:level", RequiredIntGreaterZero)
  val xp = new IntField(this,"base:xp", RequiredIntGreaterZero)
}

case class MonsterSummary(override val eid:EntityID, override val classid: EntityClassID, name:String,level:Int, xp:Int, role:String, minion:Boolean) extends EntitySummary(eid,classid)

object MonsterSummary {
  import CombatantEntityFields._
  
  private object template extends FieldSet(null) {
	val name = new StringField(this,"base:name", RequiredString)
    val role = new StringField(this,"base:role", RequiredString)
	val level = new IntField(this,"base:level", RequiredIntGreaterZero)
	val xp = new IntField(this,"base:xp", RequiredIntGreaterZero)
    val hp = new IntField(this,"stat:hp", RequiredIntGreaterZero)
  }

  def fromFieldMap(eid:EntityID,fields:Map[String,String]) = {
    template.clear()
	template.loadFromMap(fields)
    if(fields("classid") == "vcc-class:monster" && template.isValid)
      MonsterSummary(eid,
        Compendium.monsterClassID,
        template.name.value,
        template.level.value,
        template.xp.value,
        template.role.value,
        template.hp.value == 1)
    else null
  }
}

class CharacterEntity(eid:EntityID) extends CombatantEntity(eid) {
  import CombatantEntityFields._
  val combatantType = CombatantType.Character
  val classID = Compendium.characterClassID
  val charClass = new StringField(this,"base:class", RequiredString)
  val race = new StringField(this,"base:race", RequiredString)
  val level = new IntField(this,"base:level", RequiredIntGreaterZero)
}

case class CharacterSummary(override val eid:EntityID, override val classid: EntityClassID,name:String,level:Int, cclass:String, race:String) extends EntitySummary(eid,classid)

object CharacterSummary {

  import CombatantEntityFields._
  
  private object template extends FieldSet(null) {
	val name = new StringField(this,"base:name", RequiredString)
    val charClass = new StringField(this,"base:class", RequiredString)
	val race = new StringField(this,"base:race", RequiredString)
	val level = new IntField(this,"base:level", RequiredIntGreaterZero)
  }  
   
  def fromFieldMap(eid:EntityID,fields:Map[String,String]) = {
    template.clear()
    template.loadFromMap(fields)
    if(fields("classid") == "vcc-class:character" && template.isValid)  
      CharacterSummary(eid,
        Compendium.characterClassID,
        template.name.value,
        template.level.value,
        template.charClass.value,
        template.race.value)
    else null
  }
}
