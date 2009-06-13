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

abstract class Field[T](val fset:FieldContainer, val id:String) {
  
  def value:Option[T]
  
  def value_=(v:T)
  
  def clear()
  
  fset.addField(this)
  
  def fromStorageString(str:String)
  
  def toStorageString:String
  
  def prefix:String = fset.storageId + ":" + fset.storageIndex

  def extractData():List[Datum] = List(Datum(fset.storageId,fset.storageIndex,id,toStorageString))

  def toXML:scala.xml.Node = {
    val datum=toStorageString
    if(datum!=null) <datum id={id}>{datum}</datum>
    else null
  }
}
