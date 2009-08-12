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

  def valueFromStorageString(str:String):Option[E#Value] = {
    if(str==null && str=="") None
    else {
      val v=enum.valueOf(str)
      if(! v.isDefined) throw new EnumerationConstantNotPresentException(enum,str) 
      v
    }
  }
  
  override def toString = "EnumerationField("+prefix+":"+id+":= "+ value +")"
}
