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

trait FieldContainer extends DataContainer {

  private var _fields=scala.collection.immutable.Map.empty[String,Field[_]]
  
  def fields:List[Field[_]] = _fields.map(f=>f._2).toList
  
  def fieldNames:List[String] = _fields.map(f=>f._1).toList
  
  def addField(field:Field[_]) { _fields= _fields + (field.id -> field)}
  
  private[datastore] def classId():EntityClassID
  
  private[datastore] def storageId():String
  
  private[datastore] def storageIndex():Int

  private[datastore] def exportData():List[Datum] = _fields.flatMap({f=>f._2.extractData()}).toList
  
  def toXML:scala.xml.Node = <set id={id}>{_fields.map(f=>f._2.toXML).toSeq}</set>

  def loadDatum(datum:Datum) {
	if(_fields.contains(datum.key.field)) {
	  _fields(datum.key.field).fromStorageString(datum.value)
	} else {
	  throw new UnexistantField(classId,datum.key.prefix,datum.key.field)
	}
  }
  
  def getFieldFromDatumKey(key:DatumKey):Field[_] = if(_fields.contains(key.field)) _fields(key.field) else null

}

class FieldSet(val owner:Entity,val id :String) extends FieldContainer {

  owner.addContainer(this)
  
  private[datastore] def classId = owner.classId
  
  override def toString:String = "FieldSet("+id+")"
  
  private[datastore] def storageId():String = id
  
  private[datastore] def storageIndex():Int = 0

}