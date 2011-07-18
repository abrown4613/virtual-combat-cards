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
//$Id$
package vcc.scalaz

import collection.generic.Subtractable

case class Lens[S, T](get: S => T, set: (S, T) => S) {
  def compose[C](other: Lens[T, C]): Lens[S, C] = Lens(s => other.get(this.get(s)), (s, c) => this.set(s, other.set(this.get(s), c)))

  def mod(s: S, f: T => T): S = set(s, f(get(s)))

  /**
   * Modify lens if value changed. This is a helper method used to update the lens only if the value has after
   * applying change function.
   * @param s State to be changes
   * @param f Change function
   * @return New state if f(get(s)) != get(s), old value otherwise
   */
  def modIfChanged(s: S, f: T => T) = {
    val nv = f(get(s))
    if (nv != get(s)) set(s, nv)
    else s
  }
}

object Lens {

  trait WrappedLens[S, A] {
    def lens: Lens[S, A]
  }

  trait SubtractableLens[S, A, Repr <: Subtractable[A, Repr]] extends WrappedLens[S, Repr]

  implicit def mapLens[S, K, V] = MapLens[S, K, V](_)

  case class MapLens[S, K, V](lens: Lens[S, Map[K, V]]) extends SubtractableLens[S, K, Map[K, V]] {
    /**Allows both viewing and setting the value of a member of the map */

    /**This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K) = Lens[S, V](lens.get(_)(k), (s, v) => lens.mod(s, _ + (k -> v)))

  }

}
