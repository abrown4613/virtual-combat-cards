/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.domain.dndi

/**
 * Utility class for recursive reader/builders. It encapsulates a lists of tokens and works as an iterator fashion.
 * Whenever an advances is issued the derived classes can add additional logic to include special tokens.
 */
class TokenStream[T](protected var _list: List[T]) {
  private var _next: T = nextToken()

  /**
   * Return the current head of the of the stream. It will not change the information.
   */
  def head: T = _next

  /**
   * Discard current head and advance, advancing will call implementation of <code>nextToken</code>
   */
  def advance() {
    _next = nextToken
  }

  /**
   * Advances in the stream of tokens. Default implementation of this simple removes the head from the list makes then
   * next head the next token.
   * Override this to implement special logic to add tokens.
   */
  protected def nextToken(): T = {
    val ret = _list.head
    _list = _list.tail
    ret
  }

  /**
   * Returns elements that have not been processed yet.
   */
  def remainder() = _list
}
