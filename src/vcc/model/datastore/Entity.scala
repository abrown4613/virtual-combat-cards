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
//$Id$
package vcc.model.datastore

/**
 * This is the parent class for all entity summary
 */
abstract class EntitySummary(val eid:EntityID, val classid: EntityClassID)

trait EntityBuilder extends Entity {
  
  def createInstance(eid:EntityID):Entity
  
  def createSummaryFromMap(eid: EntityID, classid:EntityClassID, fmap:Map[DatumKey,String]):EntitySummary 
  
  val summaryFields:Seq[DatumKey]
}

abstract class Entity(val id:EntityID) {
  
  val classId:EntityClassID
  
  private var _containers = scala.collection.immutable.Map.empty[String,DataContainer] 
  
  val topLevel=new FieldSet(this,"base")

  def fieldSets:List[DataContainer]= _containers.map(x=>x._2).toList
  
  def addContainer(container:DataContainer) { _containers += (container.id -> container) }
  
  def extractData():List[Datum] = {
    _containers.flatMap(f=>f._2.exportData()).toList
  }
  
  def toXML:scala.xml.Node = <entity classId={classId.uri.toString} id={id.uri.toString}>{ _containers.map(c=>c._2.toXML).toSeq }</entity>
    
  def getFieldFromDatumKey(key:DatumKey):Field[_] = {
    if(_containers.isDefinedAt(key.prefix)) {
      _containers(key.prefix).getFieldFromDatumKey(key)
    } else null
  }
  def loadDatum(datum:Datum) { 
    if(_containers.contains(datum.key.prefix)) {
      _containers(datum.key.prefix).loadDatum(datum)
    } else throw new UnexistantField(classId,datum.key.prefix,datum.key.field)
  }
}
