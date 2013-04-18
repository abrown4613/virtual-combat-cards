/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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
package org.exnebula.iteratee

sealed trait ConsumerState[I, +T]

case class Continue[I, T](next: Consumer[I, T]) extends ConsumerState[I, T]

case class Done[I, T](result: T, i: Input[I]) extends ConsumerState[I, T]

case class Error[I](error: Throwable, i: Input[I]) extends ConsumerState[I, Nothing]

sealed trait Input[+E]

case class Chunk[E](elem: E) extends Input[E]

case object Empty extends Input[Nothing]

case object EOF extends Input[Nothing]


trait Consumer[I, T] {
  def consume(input: Input[I]): ConsumerState[I, T]

  def flatMap[S](f: T => Consumer[I, S])(implicit m: Manifest[S]): Consumer[I, S] = {
    val outer = this
    new Consumer[I, S] {
      def consume(i: Input[I]): ConsumerState[I, S] = outer.consume(i) match {
        case e@Error(error, input) => e
        case Continue(next) => Continue(next flatMap (f))
        case Done(result, remainder) => f(result).consume(remainder)
      }
    }
  }

  def map[S](f: T => S)(implicit m: Manifest[S]): Consumer[I, S] = {
    val outer = this
    new Consumer[I, S] {
      def consume(i: Input[I]): ConsumerState[I, S] = {
        outer.consume(i) match {
          case e@Error(error, input) => e
          case Continue(next) => Continue(next map f)
          case Done(value, remainder) => Done(f(value), remainder)
        }
      }
    }
  }

  def consumeAll(input: List[I]): (Either[Throwable, T], List[I]) = {
    consume(if (input.isEmpty) EOF else Chunk(input.head)) match {
      case Error(error, remainder) => (Left(error), input)
      case Done(returnValue, remainder) => remainder match {
        case Chunk(c) => (Right(returnValue), c :: input.tail)
        case _ => (Right(returnValue), if(input.isEmpty) Nil else input.tail)
      }

      case Continue(nextConsumer) => nextConsumer.consumeAll(input.tail)
    }
  }

  def orElse(other: Consumer[I, T]) = {
    val first = this
    new Consumer[I, T] {
      def consume(input: Input[I]): ConsumerState[I, T] = first.consume(input) match {
        case e@Error(_, remainder) => other.consume(remainder)
        case state => state
      }
    }
  }
}

object Repeat {
  def apply[I,T](group: Consumer[I, T]) = {
    val wrappedGroup = group orElse(new Consumer[I, T] {
                def consume(input: Input[I]): ConsumerState[I, T] = Error(null, input)
              })
    new Repeat(wrappedGroup, wrappedGroup, Nil)
  }
}

class Repeat[I, T] private(groupStart: Consumer[I, T], next: Consumer[I, T], acc: List[T]) extends Consumer[I, List[T]] {

  private def again(groupStart: Consumer[I, T], acc: List[T]) = new Repeat(groupStart, groupStart, acc)

  def consume(input: Input[I]): ConsumerState[I, List[T]] = input match {
    case EOF => Done(acc, EOF)
    case Empty => Continue(this)
    case Chunk(c) =>
      next.consume(input) match {
        case Error(null, remainder) => Done(acc, remainder)
        case e@Error(_, _) => e
        case Done(value, Empty) => Continue(again(groupStart, acc ::: List(value)))
        case Done(value, rest) => again(groupStart, acc ::: List(value)).consume(rest)
        case Continue(nextStep) => Continue(new Repeat(groupStart, nextStep, acc))
      }
  }
}