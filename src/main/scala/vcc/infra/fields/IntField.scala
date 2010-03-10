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
package vcc.infra.fields

class IntField(val fset:FieldSet, override val id:String, override val validator:FieldValidator[Int]) extends Field[Int](fset,id,validator) {

  override def toString = "IntField("+id+":= "+ value +")"
}

class DefaultIntFieldValidator(rules:ValidationRule[Int]*) extends FieldValidator[Int](rules: _*) {
  def fromString(str:String):FieldValue[Int] = {
    if(str!=null && str!="") {
      try {
        Defined(str.toInt)
      } catch {
        case e:NumberFormatException => Invalid(str,"'"+str+"' is not a integer number")
        case s => Invalid(str,s.getMessage)
      }
    } else Undefined
  }
}

case class BoundedInteger(range:Range) extends ValidationRule[Int] {
  def isValid(v:FieldValue[Int]):Option[String] = {
    v match {
      case Defined(iv) => if(range.contains(iv)) None else Some(iv + " not between "+ range.start + " and "+range.end ) 
      case Undefined => None
      case Invalid(raw,reason) => Some(reason)
    }
  }
}

case class IntegerGreaterThan(min:Int) extends ValidationRule[Int] {
  def isValid(v:FieldValue[Int]):Option[String] = {
    v match {
      case Invalid(raw,reason) => Some(reason)
      case Defined(iv) if(iv <= min)=> Some(iv + " must be greater than "+min) 
      case _ => None
    }
  }
}

