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

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class TokenStreamTest extends SpecificationWithJUnit with Mockito {
  "TokenStream" should {

    "throw exception if head is read prior to advance" in {
      val ts = new TokenStream[Int](List(1, 2, 3))

      ts.head must throwA[IllegalStateException]
    }

    "not advance on an empty stream" in {
      val ts = new TokenStream[Int](Nil)
      ts.advance() must beFalse
    }

    "return through if no rewrite is passed" in {
      val ts = new TokenStream[Int](List(1, 2, 3))

      ts.advance() must beTrue
      ts.head must_== 1

      ts.advance() must beTrue
      ts.head must_== 2

      ts.advance() must beTrue
      ts.head must_== 3

      ts.advance() must beFalse
    }

    "remove elemenst via filter rewrite" in {
      val ts = new TokenStream[Int](List(1, 2, 3), new ConditionalTokenStreamRewrite[Int]({
        case 2 => Nil
      }))
      streamToList(ts) must_== List(1, 3)
    }

    "remove all elemenst via filter rewrite" in {
      val mockFilter = mock[TokenStreamRewrite[Int]]
      mockFilter.rewriteToken(1) returns Nil
      mockFilter.rewriteToken(2) returns Nil
      mockFilter.rewriteToken(3) returns Nil
      val ts = new TokenStream[Int](List(1, 2, 3), mockFilter)
      streamToList(ts) must_== Nil
      there was one(mockFilter).rewriteToken(1) andThen
              one(mockFilter).rewriteToken(2) andThen
              one(mockFilter).rewriteToken(3)
    }

    "replace and add elemenst via filter rewrite" in {
      val ts = new TokenStream[Int](List(1, 2, 3), new ConditionalTokenStreamRewrite[Int]({
        case 2 => List(4, 8)
      }))
      streamToList(ts) must_== List(1, 4, 8, 3)
    }

    "replace and remove elemenst via filter rewrite" in {
      val ts = new TokenStream[Int](List(1, 2, 3), new ConditionalTokenStreamRewrite[Int]({
        case 2 => List(4, 8)
        case 3 => Nil
      }))
      streamToList(ts) must_== List(1, 4, 8)
    }
  }

  def streamToList[T](ts: TokenStream[T]): List[T] = {
    val list = scala.collection.mutable.ListBuffer.empty[T]
    while (ts.advance) list += ts.head
    list.toList
  }
}