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
trait CommandStream[S, A] {

  /**
   * Create a composed stream, the current stream is the first one, once it provides no commands the next stream will be
   * tested for commands.
   * @param that Stream to check if current stream yields not result.
   * @return Composed stream
   */
  def followedBy(that: CommandStream[S, A]): CommandStream[S, A] = ChainedCommandStream(this, that)

  /**
   * Get next command, based on a given state, if there is a command for the state Some will be returned, None indicates
   * that the Stream has no command for the give state.
   * @param s State to get the command for
   * @return Option of pair of value and next Stream to use on next call, user should always use the returned stream since it may
   * be different form the original one.
   */
  def get(s: S): Option[(A, CommandStream[S, A])]
}

object CommandStream {
  /**
   * Build a command stream from a sequence of commands
   */
  def apply[S, A](elements: A*):CommandStream[S, A] = SeqCommandStream[S, A](elements)
}

/**
 * This is a wrapper to generates commands based on the a Seq. State is ignored for all method calls.
 * @param elem Sequence of elements this stream will return in order
 */
case class SeqCommandStream[S, A](elem: Seq[A]) extends CommandStream[S, A] {

  /**
   * This implementation will return the next available element in the Seq, and a new Stream with the remainder of the
   * elements of the Seq
   * @see vcc.tracker.CommandStream#get
   */
  def get(s: S): Option[(A, CommandStream[S, A])] = {
    if (!elem.isEmpty) Some((elem.head, SeqCommandStream[S, A](elem.tail)))
    else None
  }
}

/**
 * Create a stream based on a partial function. The stream will provide commands if the partial function is defined at
 * the supplied state.
 * @param pf Partial function mapping a given state to commands
 */
class PartialFunctionCommandStream[S, A](pf: PartialFunction[S, A]) extends CommandStream[S, A] {

  /**
   * Returns the result of the application the state to the objects pf variable. If the function is note defined at
   * that state an exception is thrown.
   * @see vcc.tracker.CommandStream#get
   */
  def get(s: S): Option[(A, CommandStream[S, A])] = pf.lift(s).map(x => (x, this))
}

/**
 * This construct links two CommandStreams to as a stream.
 */
case class ChainedCommandStream[S, A](first: CommandStream[S, A], second: CommandStream[S, A]) extends CommandStream[S, A] {

  /**
   * Returns the result of the first stream if there is a command defined for the specified state, if there are no
   * commands use the second stream.
   * @see vcc.tracker.CommandStream#get
   */
  override def get(s: S) = first.get(s).map(p => (p._1, ChainedCommandStream(p._2, second))).orElse(second.get(s))
}