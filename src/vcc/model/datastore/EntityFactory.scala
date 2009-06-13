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

/**
 * 
 */
object EntityFactory {
  
  type Constructor = String => Entity
  
  private var _registry=scala.collection.immutable.Map.empty[String,Constructor]
  
  /**
   * Register a class and constructor for a given class. Will replace prior definition
   * @param classId The name of the class to be registers
   * @param constructor A function that returns of Entity of 
   */
  def registerEntityClass(classId:String,constructor:Constructor) {
    _registry += classId->constructor
  }
  
  /**
   * Check if the class is registered on the Factory
   * @parma classId Class name to get
   * @return true if the class is register
   */
  def isClassDefined(classId:String):Boolean = _registry.contains(classId)
  
  /**
   * Create a new instance of the class if it's registered
   */
  def createInstance(classId:String, id:String):Entity = {
    if(isClassDefined(classId)) _registry(classId)(id)
    else null
  }

}
