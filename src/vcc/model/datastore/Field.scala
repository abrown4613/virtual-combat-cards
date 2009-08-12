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

trait ValidationError

class NotProperFormatForFieldError(e:Throwable) extends ValidationError

abstract class Field[T](val fset:FieldContainer, val id:String) {
  
  protected var _value:Option[T] = None
  
  def value:Option[T] = _value
  
  def value_=(v:T) { _value = Some(v)}
  
  def clear() { _value = None }
  
  fset.addField(this)
  
  def valueFromStorageString(str:String):Option[T]
  
  def fromStorageString(str:String) {
    _value = valueFromStorageString(str)
  }
  
  def toStorageString:String = if(_value.isDefined) _value.get.toString else null
  
  def prefix:String = fset.storageId + ":" + fset.storageIndex
  
  val datumKey:DatumKey = DatumKey(fset.storageId,fset.storageIndex,id)

  def extractData():List[Datum] = List(Datum(datumKey,toStorageString))

  def toXML:scala.xml.Node = {
    val datum=toStorageString
    if(datum!=null) (<datum id={id}>{datum}</datum>)
    else null
  }
  
  def isValid:Boolean = (validate(_value) == None)
  
  /**
   * This is a validation for the field. This method can be oveloaded and 
   * stacked to make for complex validation rules. 
   * Unless validation traits are added a field is always valid if it exists.
   * @param value Value to be checked, this is already in the appropriate type system
   * @return None if the value is ok, or the validation error is something failed
   */
  def validate(value: Option[T]):Option[ValidationError] = None
  
  /**
   * This is a helper method to check a string against the format
   * and validation of a give field.
   */
  def inputValidation(str:String):Option[ValidationError] = {
    try {
      val v = valueFromStorageString(str)
      validate(v)
    } catch {
      case s => Some(new NotProperFormatForFieldError(s))
    }
  }  
}
