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

class StringField(override val fset:FieldContainer, override val id:String) extends Field[String](fset,id)  {

  var _value:String=null

  def value_=(value:String) { _value=value }
  
  def value: Option[String] = if(_value!=null) Some(_value) else None
  
  def clear() { _value = null }
  
  def fromStorageString(str:String) {
    _value=str
  }
  
  def toStorageString:String = _value
  
  override def toString:String = "StringField("+prefix +":"+id+ ":="+ value +")"
}
