/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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

trait ValidationRule[T] {
  def isValid(v:FieldValue[T]):Option[String]
} 

abstract class FieldValidator[T](rules:ValidationRule[T]*) {
  
  protected var validationRules:List[ValidationRule[T]] = rules.toList
  
  def fromString(raw:String):FieldValue[T]
  
  def validate(raw:String):FieldValue[T] = {
    val v = fromString(raw)
    if(v.isValid) {
      for(rule<-validationRules) {
        rule.isValid(v) match {
          case None => 
          case Some(problem) => return Invalid(raw,problem)
        }
      }
    }
    v
  }
  
  def addRule(rule:ValidationRule[T]) { 
    validationRules = validationRules ::: List(rule)
  }
}

case class Mandatory[T]() extends ValidationRule[T] {
  def isValid(v:FieldValue[T]):Option[String] = 
    v match {
      case Defined(_) => None
      case _ => Some("Field cannot be undefined")
    }
}