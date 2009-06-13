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

class EnumerationConstantNotPresentException(enum:Enumeration,str:String) extends Exception {
  override def getMessage() = "Enumeration "+enum.getClass.getCanonicalName+ " does not contains value " + str
}

class EnumerationField[E<:Enumeration](fset:FieldContainer,id:String,enum:E) extends Field[E#Value](fset,id){

  private var _value:Option[E#Value]=None
  
  def value_= (v:E#Value) { _value=Some(v) }
  
  def clear { _value = None }
  
  def value:Option[E#Value] = _value
  
  def fromStorageString(str:String) {
    val v=enum.valueOf(str)
    if((str!=null && str!="") && ! v.isDefined) 
      throw new EnumerationConstantNotPresentException(enum,str) 
    _value=v
  }
  
  def toStorageString:String = if(_value.isDefined) _value.get.toString else null
  
  override def toString = "IntField("+prefix+":"+id+":= "+ value +")"
}
