/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dndi.reader

import vcc.dndi.reader.Parser.BlockElement
import org.exnebula.iteratee._
import scala.Error
import org.exnebula.iteratee.Done
import org.exnebula.iteratee.Chunk
import org.exnebula.iteratee.Continue

trait BlockElementConsumer {
  protected def matchConsumer[T](expected: String)(matcher: PartialFunction[BlockElement, T]) = new Consumer[BlockElement, T] {
    def consume(input: Input[BlockElement]): ConsumerState[BlockElement, T] = input match {
      case Chunk(c) if (matcher.isDefinedAt(c)) => Done(matcher(c), Empty)
      case Chunk(c) => Error(new UnexpectedBlockElementException(expected + " expected", c), input)
      case EOF => Error(new UnexpectedBlockElementException(expected + " expected got EOF", null), EOF)
      case Empty => Continue(this)
    }
  }
  protected def dropWhile[I](p: I => Boolean) = new Consumer[I, Unit] {
    def consume(input: Input[I]): ConsumerState[I, Unit] = input match {
      case EOF => Done((), EOF)
      case Empty => Continue(this)
      case Chunk(c) if (p(c)) => Continue(this)
      case Chunk(c) => Done((), input)
    }
  }
}