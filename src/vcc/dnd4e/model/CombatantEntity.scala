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

import vcc.model.datastore._
import vcc.model.Registry

abstract class CombatantEntity(val eid:EntityID) extends Entity(eid) {
  def combatantType: CombatantType.Value
  
  val name = new StringField(topLevel,"name")
  
  val stats = new FieldSet(this,"stats")
  val text = new FieldSet(this,"text")
  
  val initiative = new IntField(stats,"initiative")
  val hp = new IntField(stats,"hp")

  val ac = new IntField(stats,"ac")
  val fortitude = new IntField(stats,"fortitude")
  val reflex = new IntField(stats,"reflex")
  val will = new IntField(stats,"will") 
  
  val statblock = new StringField(text,"statblock")
}

class MonsterEntity(eid:EntityID) extends CombatantEntity(eid) {
  val classId = Compendium.monsterClassID
  def combatantType = if(hp==1) CombatantType.Minion else CombatantType.Monster
  
  val role = new StringField(topLevel,"role")
  val level = new IntField(topLevel,"level")
  val xp = new IntField(topLevel,"xp")
  
}

object MonsterEntityBuilder extends MonsterEntity(null) with EntityBuilder {

  def createInstance(eid:EntityID):Entity = new MonsterEntity(eid)
  
  def createSummaryFromMap(eid: EntityID, classid:EntityClassID, fmap:Map[DatumKey,String]):EntitySummary = 
    if(!fmap.isEmpty)
      MonsterSummary(eid,classid,fmap(name.datumKey),fmap(level.datumKey).toInt, fmap(xp.datumKey).toInt,fmap(role.datumKey),(fmap(hp.datumKey)=="1"))
    else null

  val summaryFields = Seq(name,level,xp,role,hp).map(_.datumKey)
  
}

case class MonsterSummary(override val eid:EntityID,override val classid: EntityClassID, name:String,level:Int, xp:Int, role:String, minion:Boolean) extends EntitySummary(eid,classid)

class CharacterEntity(eid:EntityID) extends CombatantEntity(eid) {
  val classId = Compendium.characterClassID
  val combatantType = CombatantType.Character
  val charClass = new StringField(topLevel,"class")
  val race = new StringField(topLevel,"race")
  val level = new IntField(topLevel,"level")
}

object CharacterEntityBuilder extends CharacterEntity(null) with EntityBuilder {

  def createInstance(eid:EntityID):Entity = new CharacterEntity(eid)
  
  def createSummaryFromMap(eid: EntityID, classid:EntityClassID, fmap:Map[DatumKey,String]):EntitySummary = 
    if(!fmap.isEmpty)
      CharacterSummary(eid,classid,fmap(name.datumKey),fmap(level.datumKey).toInt, fmap(charClass.datumKey),fmap(race.datumKey))
    else null
  
  val summaryFields = Seq(name,level,charClass,race).map(_.datumKey)
}

case class CharacterSummary(override val eid:EntityID,override val classid: EntityClassID,name:String,level:Int, cclass:String, race:String) extends EntitySummary(eid,classid)

