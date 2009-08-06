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

class MultiSetFieldContainer(val parent:MultiSet[_]) extends FieldContainer {
  
  override def id:String = storageIndex().toString
  
  private[datastore] def storageId():String = parent.id
  
  private[datastore] def storageIndex():Int = parent.indexOf(this)

  private[datastore] def classId():EntityClassID = parent.owner.classId

}

class MultiSet[T<:MultiSetFieldContainer](val owner:Entity,val id:String,constructor:()=>T) extends DataContainer {
  var _elem:List[T]=Nil
  
  owner.addContainer(this)
  
  def apply(idx:Int):T =  _elem(idx)
  
  def addInstance():Int = {
    _elem = _elem ::: List(constructor())
    _elem.length-1
  }
  
  def indexOf(elem:MultiSetFieldContainer) = _elem.indexOf(elem)
  
  override def exportData():List[Datum] = {
    _elem.flatMap(e=>e.exportData())
  }
  
  override def toString():String = "MultiSet("+id+")"
  
  def toXML:scala.xml.Node= <mset id={id}>{_elem.map(e=>e.toXML).toSeq}</mset>

  def loadDatum(datum:Datum) {
    //Fill up table
    while(_elem.length-1 < datum.key.index) addInstance()
    _elem(datum.key.index).loadDatum(datum)
  }
  
  def getFieldFromDatumKey(key:DatumKey):Field[_] = if(_elem.size <= key.index) _elem(key.index).getFieldFromDatumKey(key) else null

}
