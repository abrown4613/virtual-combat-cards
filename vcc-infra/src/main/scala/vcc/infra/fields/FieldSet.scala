/**
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

package vcc.infra.fields

import vcc.infra.datastore.naming._
import vcc.infra.datastore.DataStoreEntity

import org.slf4j.Logger

abstract class FieldSet(eid:EntityID) {
  
  private var fieldSet = Map.empty[String,Field[_]]
  
  private [fields] def addField(field:Field[_]) {
    if(fieldSet.isDefinedAt(field.id)) throw new Exception("Can not repeat field in FieldSet")
    fieldSet = fieldSet + (field.id -> field)
  }
  
  def asDataStoreEntity():DataStoreEntity = {
    DataStoreEntity(eid,Map(fieldSet.map(f=>(f._1 -> f._2.storageString)).filter(x => x._2 != null).toSeq: _*))
  }
  
  /**
   * Load and valid fields from a map
   */
  def loadFromMap(map:Map[String,String]):Boolean = {
    for((k,v)<-map) {
      if(fieldSet.contains(k)) k->fieldSet(k).fromStorageString(v)
    }
    isValid()
  }
  
  def isValid():Boolean = fieldSet.foldLeft(true)((p,kp)=>p && kp._2.isValid)
  
  def dump(log:Logger) {
	log.debug("Entity {} contains:",eid)
	for((k,v)<-fieldSet) {
	  log.debug("  - {} = {} ",k,v.storageString)
	}
  }
  
  def clear() {
    fieldSet.values.foreach(x=> x.fromStorageString(null))
  }
}
