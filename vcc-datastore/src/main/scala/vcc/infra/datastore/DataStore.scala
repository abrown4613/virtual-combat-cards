/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.datastore

import naming._

import java.net.URI

case class DataStoreEntity(eid: EntityID, data: Map[String, String])

class DataStoreIOException(msg: String, exception: Throwable) extends Exception(msg, exception)

trait DataStore {

  def loadEntity(eid: EntityID): DataStoreEntity

  def storeEntity(ent: DataStoreEntity): Boolean

  def entityTimestamp(eid: EntityID): Long

  def enumerateEntities(): Seq[EntityID]

  def extractEntityData(keys: Set[String]): Seq[(EntityID, Map[String, String])]

  def entityExists(eid: EntityID): Boolean

  def deleteEntity(eid: EntityID): Boolean

  def close()

}

trait DataStoreBuilder {

  def open(esid: DataStoreURI): DataStore

  def create(esid: DataStoreURI): DataStore

  def exists(esid: DataStoreURI): Boolean

  def destroy(esid: DataStoreURI): Boolean

  def isResolvedDataStoreURI(esid: DataStoreURI): Boolean

  def resolveDataStoreURI(dsu: DataStoreURI, replace: Map[String, URI]): DataStoreURI
}


class DataStoreFactoryException(msg: String, t: Throwable) extends Exception(msg, t)

object DataStoreFactory {

  private var builders = Map.empty[String, DataStoreBuilder]

  private def newInstance[T <: AnyRef](className: String): T = {
    try {
      val objClass = this.getClass.getClassLoader.loadClass(className)
      if (objClass != null) {
        objClass.newInstance.asInstanceOf[T]
      } else {
        throw new DataStoreFactoryException("Failed to find class " + className + " there is now", null)
      }
    } catch {
      case e:Exception =>
        throw new DataStoreFactoryException("Failed to find class " + className, e)
    }
  }

  def getDataStoreBuilder(esid: DataStoreURI): DataStoreBuilder = {
    if (builders.isDefinedAt(esid.subScheme)) builders(esid.subScheme)
    else {
      val className = "vcc.infra.datastore." + esid.subScheme + ".StoreBuilder"
      val build = newInstance[DataStoreBuilder](className)
      builders = builders + (esid.subScheme -> build)
      build
    }
  }
}