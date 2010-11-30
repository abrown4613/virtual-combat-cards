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
package vcc.model

import scala.reflect.Manifest

/**
 * The Registry is a singleton object responsible for storing any kind of object
 * based on a key. To retrieve the object from the registry you must use specify 
 * the correct type parameter.
 *
 * This code is based on:
 * http://dcsobral.blogspot.com/2009/07/getting-around-type-erasure-with.html
 */
class Registry[K] {

  private var _map = Map.empty[K, (Manifest[_], Any)]

  /**
   * Register an object on the Registry. It will implicitly store the object
   * Manifest for proper type matching upon retrieval.
   * @param key The key object of value to search for
   * @param item The object being stored 
   *
   */
  def register[T](key: K, item: T)(implicit m: Manifest[T]) {
    _map = _map.updated(key, (m, item))
  }

  /**
   * Retrieve an object from the registry. The object must be 
   * castable to the type parameter specified.
   * @param key The key to search for, can be Any object or type
   * @return The object if found and matches the type parameter, or None otherwise.
   */
  def get[T](key: K)(implicit m: Manifest[T]): Option[T] = {
    val o = _map.get(key)

    o match {
      case Some((om: Manifest[_], s: Any)) =>
        if (om <:< m) Some(s.asInstanceOf[T]) else None
      case _ => None
    }
  }

  /**
   * Checks the existence of a given key on the registry (irrespective of Manifest type).
   * @param key Key to lookup
   * @return True if an entry with that key exists in the registry. False otherwise.
   */
  def contains(key:K):Boolean = _map.contains(key)
}

/**
 * Singleton Registry that hold any type of object with manifest.
 */
object Registry {
  private val _registry = new Registry[Any]()

  /**
   * Retrieve an object from the registry. The object must be
   * castable to the type parameter specified.
   * @param key The key to search for, can be Any object or type
   * @return The object if found and matches the type parameter, or None otherwise.
   */
  def register[T](key: Any, item: T)(implicit m: Manifest[T]) {
    _registry.register(key, item)(m)
  }

  /**
   * Retrieve an object from the registry. The object must be
   * castable to the type parameter specified.
   * @param key The key to search for, can be Any object or type
   * @return The object if found and matches the type parameter, or None otherwise.
   */
  def get[T](key: Any)(implicit m: Manifest[T]): Option[T] = _registry.get[T](key)(m)

}
