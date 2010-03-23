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
package vcc.model

/**
 * The Registry is a singleton object responsible for storing any kind of object
 * based on a key. To retrieve the object from the registry you must use specify 
 * the correct type parameter.
 * 
 * This code is based on:
 * http://dcsobral.blogspot.com/2009/07/getting-around-type-erasure-with.html
 */
object Registry {
  
  import scala.reflect.Manifest
  
  private var _map= Map.empty[Any,(Manifest[_], Any)] 
  
  /**
   * Register an object on the Registry. It will implicitly store the object
   * Manifest for proper type matching upon retrieval.
   * @param key The key object of value to search for
   * @param item The object being stored 
   * 
   */
  def register[T](key: Any, item: T)(implicit m: Manifest[T]) {
    _map = _map(key) = (m, item)
  }
  
  /**
   * Retrieve an object from the registry. The object must be 
   * castable to the type parameter specfied.
   * @param key The key to search for, can be Any object or type
   * @return The object if found and matches the type parameter, or None otherwise.
   */
  def get[T](key:Any)(implicit m : Manifest[T]): Option[T] = {
    val o = _map.get(key)
   
    o match {
      case Some((om: Manifest[_], s : Any)) =>
        if(om <:< m) Some(s.asInstanceOf[T]) else None
      case _ => None
    }
  }
}
