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

class StringField(val fset:FieldSet, override val id:String,override val validator:FieldValidator[String])
        extends Field[String](fset,id,validator)  {
  override def toString:String = "StringField("+id+ ":="+ value +")"
}

class DefaultStringFieldValidator(rules:ValidationRule[String]*) extends FieldValidator[String](rules: _*) {
  def fromString(str:String):FieldValue[String] = if(str == null || str == "") Undefined else Defined(str)
}