/*
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.dnd4e.domain.compendium

import vcc.infra.datastore.naming._
import vcc.infra.fields._
import vcc.dnd4e.tracker.common.CombatantType

class MonsterEntity(eid: EntityID) extends CombatantEntity(eid) {

  import CombatantEntityFields._

  val classID = Compendium.monsterClassID

  def combatantType = if (hp == 1) CombatantType.Minion else CombatantType.Monster

  val role = new StringField(this, "base:role", RequiredString)
  val level = new IntField(this, "base:level", RequiredIntGreaterZero)
  val xp = new IntField(this, "base:xp", RequiredInt)

  protected def createInstance(eid: EntityID) = new MonsterEntity(eid)
}

object MonsterEntity {
  def newInstance() = new MonsterEntity(EntityID.generateRandom())

  /**
   * Added a monster based ID to this entity. Will always generate the same UUID.
   * @para dndID The DNDID of the monster
   */
  def newInstance(dndID: Int) = new MonsterEntity(EntityID.fromName("dndi:monster:" + dndID))

}

case class MonsterSummary(override val eid: EntityID, override val classid: EntityClassID, name: String, level: Int, xp: Int, role: String, minion: Boolean) extends EntitySummary(eid, classid)

object MonsterSummary {

  import CombatantEntityFields._

  private object template extends FieldSet(null) {
    val name = new StringField(this, "base:name", RequiredString)
    val role = new StringField(this, "base:role", RequiredString)
    val level = new IntField(this, "base:level", RequiredIntGreaterZero)
    val xp = new IntField(this, "base:xp", RequiredInt)
    val hp = new IntField(this, "stat:hp", RequiredIntGreaterZero)
  }

  def fromFieldMap(eid: EntityID, fields: Map[String, String]) = {
    template.clear()
    template.loadFromMap(fields)
    //TODO This will not show invalid base fields
    if (fields("classid") == Compendium.monsterClassIDStorageString && template.isValid)
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
