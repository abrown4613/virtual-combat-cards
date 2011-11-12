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
package vcc.dnd4e.compendium

import vcc.infra.datastore.naming._
import vcc.infra.fields._

class CharacterEntity(eid: EntityID) extends CombatantEntity(eid) {

  import CombatantEntityFields._

  val combatantType = CombatantType.Character
  val classID = Compendium.characterClassID
  val charClass = new StringField(this, "base:class", RequiredString)
  val race = new StringField(this, "base:race", RequiredString)
  val level = new IntField(this, "base:level", RequiredIntGreaterZero)
  val perception = new IntField(this, "skill:perception", AnyInt)
  val insight = new IntField(this, "skill:insight", AnyInt)
  val senses = new StringField(this, "base:senses", AnyString)

  protected def createInstance(eid: EntityID): CombatantEntity = new CharacterEntity(eid)
}

object CharacterEntity {
  def newInstance(): CharacterEntity = new CharacterEntity(EntityID.generateRandom())
}

case class CharacterSummary(override val eid: EntityID, override val classid: EntityClassID, name: String, level: Int, cclass: String, race: String) extends EntitySummary(eid, classid)

object CharacterSummary {

  import CombatantEntityFields._

  private object template extends FieldSet(null) {
    val name = new StringField(this, "base:name", RequiredString)
    val charClass = new StringField(this, "base:class", RequiredString)
    val race = new StringField(this, "base:race", RequiredString)
    val level = new IntField(this, "base:level", RequiredIntGreaterZero)
  }

  def fromFieldMap(eid: EntityID, fields: Map[String, String]) = {
    template.clear()
    template.loadFromMap(fields)
    if (fields("classid") == Compendium.characterClassIDStorageString && template.isValid)
      CharacterSummary(eid,
        Compendium.characterClassID,
        template.name.value,
        template.level.value,
        template.charClass.value,
        template.race.value)
    else null
  }
}
