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

/**
 * This package include general logic and supporting classes for dispatching commands to tracker and converting it into
 * a sequence of Transitions to update the tracker State.
 */
package vcc.tracker

/**
 * A CommandStream is a source for some type of command (type A) based on a the state (type S). This can be used to
 * generate sequences of messages depending on the state of the tracker. It is the reponsability of the caller to ensure
 * that the correct state is provided on each call to the function.
 */
trait CommandStream[A, S] {

  /**
   * Indicate whether or not the stream has some command for a given state.
   * @param s State to check against, notices that different states may yield different result for the same stream.
   * @return true if you can safely request a command from this stream, false if the stream has not commands for the
   * given state.
   */
  def hasNext(s: S): Boolean

  /**
   * Get next command, based on a given state. Client should always call hasNext with the give state
   * @param s State to get the command for
   * @return Pair of value and next Stream to use on next call, user should always use the returned stream since it may
   * be different form the original one.
   * @throw NoSuchElementException If the at the supplied state the stream has no command, it will throw this exceptions.
   * Messages vary according to implementation.
   */
  @throws(classOf[NoSuchElementException])
  def next(s: S): (A, CommandStream[A, S])

  /**
   * Create a composed stream, the current stream is the first one, once it provides no commands the next stream will be
   * tested for commands.
   * @param that Stream to check if current stream yields not result.
   * @return Composed stream
   */
  def followedBy(that: CommandStream[A, S]) = ChainedCommandStream(this, that)
}

/**
 * This is a wrapper to generates commands based on the a Seq. State is ignored for all method calls.
 * @param elem Sequence of elements this stream will return in order
 */
case class SeqCommandStream[A, S](elem: Seq[A]) extends CommandStream[A, S] {
  /**
   * This implementation will return the next available element in the Seq, and a new Stream with the remainder of the
   * elements of the Seq
   * @see vcc.tracker.CommandStream#next
   */
  def next(s: S): (A, CommandStream[A, S]) = (elem.head, SeqCommandStream(elem.tail))

  /**
   * @see vcc.tracker.CommandStream#hasNext
   */
  def hasNext(s: S): Boolean = !elem.isEmpty
}

/**
 * Create a stream based on a partial function. The stream will provide commands if the partial function is defined at
 * the supplied state.
 * @param pf Partial function mapping a given state to commands
 */
class PartialFunctionCommandStream[A, S](pf: PartialFunction[S, A]) extends CommandStream[A, S] {

  /**
   * Returns the result of the application the state to the objects pf variable. If the function is note defined at
   * that state an exception is thrown.
   * @see vcc.tracker.CommandStream#next
   */
  def next(s: S): (A, CommandStream[A, S]) = {
    if (pf.isDefinedAt(s)) (pf.apply(s), this)
    else throw new NoSuchElementException("CommandStream not defined at " + s)
  }

  /**
   * True if the partial function is defined at this state.
   * @see vcc.tracker.CommandStream#hasNext
   */
  def hasNext(s: S): Boolean = pf.isDefinedAt(s)
}

/**
 *
 */
case class ChainedCommandStream[A, S](first: CommandStream[A, S], second: CommandStream[A, S]) extends CommandStream[A, S] {

  def next(s: S): (A, CommandStream[A, S]) = if (first.hasNext(s)) {
    val (v, a) = first.next(s)
    (v, ChainedCommandStream(a, second))
  } else {
    second.next(s)
  }

  def hasNext(s: S): Boolean = first.hasNext(s) || second.hasNext(s)
}