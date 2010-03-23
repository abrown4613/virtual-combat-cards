//$Id$
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

package vcc.infra.fields

abstract class Field[T](val fieldSet:FieldSet, val id:String, val validator:FieldValidator[T]) {
  
  protected var _value:FieldValue[T] = validator.validate(null)
  
  //Must add to FieldSet
  fieldSet.addField(this)
  
  def value:T = _value.get
  
  def isDefined:Boolean = _value.isDefined
  
  def value_=(v:T) { _value = Defined(v)}
  
  def fieldValue = _value
  
  def clear() { _value = Undefined }
  
  def fromStorageString(str:String) {
    _value = validator.validate(str)
  }
  
  def storageString:String = _value.storageString

  def isValid:Boolean = _value.isValid
  
}
