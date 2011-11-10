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
package vcc.dnd4e.domain.compendium

import vcc.infra.datastore.naming._
import vcc.infra.fields._

class TrapEntity(eid: EntityID) extends MonsterEntity(eid) {

  import CombatantEntityFields._

  override val classID = Compendium.trapClassID

  val trapClass = new StringField(this, "base:class", RequiredString)

  override protected def createInstance(eid: EntityID) = new TrapEntity(eid)
}

object TrapEntity {
  def newInstance() = new TrapEntity(EntityID.generateRandom())

  /**
   * Added a monster based ID to this entity.
   */
  def newInstance(dndID: Int) = new TrapEntity(EntityID.fromName("dndi:trap:" + dndID))

}

case class TrapSummary(override val eid: EntityID, override val classid: EntityClassID, name: String, level: Int, xp: Int, role: String, hazard: Boolean) extends EntitySummary(eid, classid)

object TrapSummary {

  import CombatantEntityFields._

  private object template extends FieldSet(null) {
    val name = new StringField(this, "base:name", RequiredString)
    val role = new StringField(this, "base:role", RequiredString)
    val level = new IntField(this, "base:level", RequiredIntGreaterZero)
    val xp = new IntField(this, "base:xp", RequiredIntGreaterZero)
    val trapClass = new StringField(this, "base:class", RequiredString)
  }

  def fromFieldMap(eid: EntityID, fields: Map[String, String]) = {
    template.clear()
    template.loadFromMap(fields)
    //TODO This will not show invalid base fields
    if (fields("classid") == Compendium.trapClassIDStorageString && template.isValid)
      TrapSummary(eid,
        Compendium.trapClassID,
        template.name.value,
        template.level.value,
        template.xp.value,
        template.role.value,
        "hazard".equalsIgnoreCase(template.trapClass.value))
    else null
  }
}

