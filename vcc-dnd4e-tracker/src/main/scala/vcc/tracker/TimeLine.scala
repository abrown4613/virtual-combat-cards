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
package vcc.tracker

object TimeLine {

  class OutOfBounds extends RuntimeException

}

class TimeLine[S] {

  private var state: Option[S] = None
  private var past: List[S] = Nil

  private var future: List[S] = Nil

  def hasFuture = !future.isEmpty

  def hasPast = !past.isEmpty

  def forwardState() {
    throwOutOfBoundsEmptyList(future)
    past = state.toList ::: past
    state = future.headOption
    future = future.tail
  }

  def revertState() {
    throwOutOfBoundsEmptyList(past)
    future = state.toList ::: future
    state = past.headOption
    past = past.tail
  }

  def store(newState: S) {
    past = state.toList ::: past
    state = Some(newState)
    future = Nil
  }

  def forgetPast() {
    past = Nil
  }

  def getState: Option[S] = state

  private def throwOutOfBoundsEmptyList(list: List[S]) {
    if (list.isEmpty)
      throw new TimeLine.OutOfBounds
  }

}