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
//$Id$
package test.helper

/**
 * This create an object with two extractors to be used in matching
 * and converting data form arbitraty sequences
 * @param matcher A matcher partial function, it should return the entry it matches 
 * by the isDefinedAt method, converted to the type of the Matcher
 * 
 */
class Matcher [T](matcher:PartialFunction[Any,T]) {
  
  /**
   * Find and return the first matching pattern, this is
   * done using a for and will return just the first case.
   */
  object findFirst {
    def unapply(s:Seq[Any]):Option[T] = {
      for(x<-s) if(matcher.isDefinedAt(x)) return Some(matcher(x))
      None
    }
  }
  /**
   * Extractor for a sequence of matching elements, 
   * the extrator will return empty sequences if no 
   * match is found
   */
  object findAll {
    def unapplySeq(s:Seq[Any]):Option[Seq[T]]= {
      Some(s.filter(x=>{matcher.isDefinedAt(x)}).map(matcher))
    }
  }
}
