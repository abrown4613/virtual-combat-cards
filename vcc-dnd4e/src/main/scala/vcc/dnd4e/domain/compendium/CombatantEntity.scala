/**
 *   Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
import vcc.infra.datastore.DataStoreEntity
import vcc.infra.fields._
import vcc.dnd4e.tracker.common.CombatantType

case class EntityClassID(uri: java.net.URI) {
  def shortClassName() = uri.getSchemeSpecificPart()
}

abstract class EntitySummary(val eid: EntityID, val classid: EntityClassID)

object CombatantEntityFields {

  object RequiredString extends DefaultStringFieldValidator(Mandatory())

  object RequiredIntGreaterZero extends DefaultIntFieldValidator(Mandatory(), IntegerGreaterThan(0))

  object RequiredInt extends DefaultIntFieldValidator(Mandatory())

  object AnyInt extends DefaultIntFieldValidator()

  object AnyString extends DefaultStringFieldValidator()

}

abstract class CombatantEntity(val eid: EntityID) extends FieldSet(eid) {

  import CombatantEntityFields._

  val classID: EntityClassID

  def combatantType: CombatantType.Value

  val name = new StringField(this, "base:name", RequiredString)

  val initiative = new IntField(this, "stat:initiative", RequiredInt)
  val hp = new IntField(this, "stat:hp", RequiredIntGreaterZero)

  val ac = new IntField(this, "stat:ac", AnyInt)
  val fortitude = new IntField(this, "stat:fortitude", AnyInt)
  val reflex = new IntField(this, "stat:reflex", AnyInt)
  val will = new IntField(this, "stat:will", AnyInt)

  val statblock = new StringField(this, "text:statblock", AnyString)
  val comment = new StringField(this, "text:comment", AnyString)

  override def asDataStoreEntity(): DataStoreEntity = {
    val es = super.asDataStoreEntity
    DataStoreEntity(es.eid, es.data + ("classid" -> classID.uri.toString))
  }
}

/**
 * This service will create a combatant entity from a DataStoreEntity
 */
object CombatantEntityBuilder {
  protected val entityFactory: PartialFunction[(EntityID, String), CombatantEntity] = {
    case (eid, Compendium.monsterClassIDStorageString) => new MonsterEntity(eid)
    case (eid, Compendium.characterClassIDStorageString) => new CharacterEntity(eid)
    case (eid, Compendium.trapClassIDStorageString) => new TrapEntity(eid)
  }

  /**
   * Inform if this type of DataStoreEntity is can be built by this builder.
   * @param dse The Entity you wish to build
   * @return true if it can be built
   */
  def canHandle(dse: DataStoreEntity) = entityFactory.isDefinedAt(null, dse.data.getOrElse("classid", null))

  /**
   * Build the entity
   * @param dse must be a non-null entity that can be handled (see <code>canHandle</code>)
   * @return A valid entity or null otherwise
   */
  def buildEntity(dse: DataStoreEntity): CombatantEntity = {
    if (dse != null && canHandle(dse)) {
      val ent = entityFactory(dse.eid, dse.data("classid"))
      ent.loadFromMap(dse.data)
      ent
    } else {
      null
    }
  }
}