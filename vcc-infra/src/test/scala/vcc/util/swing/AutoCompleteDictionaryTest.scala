/*
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
//$Id
package vcc.util.swing

import org.specs.SpecificationWithJUnit
import java.io.ByteArrayInputStream
import org.specs.mock.Mockito

class AutoCompleteDictionaryTest extends SpecificationWithJUnit with Mockito {

  "AutoCompleteDictionary class" should {
    val dict = new AutoCompleteDictionary(List("","test","tea"))

    "return blank prefix is need" in {
      dict.findSuggestion("") must_== Some("")
    }

    "return first instance" in {
      dict.findSuggestion("te") must_== Some("test")
    }

    "return full word" in {
      dict.findSuggestion("tea") must_== Some("tea")
    }

    "return first instance" in {
      dict.findSuggestion("dagger") must_== None
    }
  }

  "AutoCompleteDictionary object" should {
    val callback = mock[(String,String)=>Unit]

    "ignore blank lines and \\r" in {
      val winBuffer = new ByteArrayInputStream("first\n\rsecond\n\rsector\n\r ".getBytes())

      val dict = AutoCompleteDictionary.fromStream(winBuffer,callback)
      there was no(callback).apply(any[String],any[String])
      dict.findSuggestion(" ") must_== None
      dict.findSuggestion("") must_== Some("first")
    }

    "ignore blank lines work with linux format" in {
      val linuxBuffer = new ByteArrayInputStream("first\nsecond\nsector\n ".getBytes())
      val dict = AutoCompleteDictionary.fromStream(linuxBuffer,callback)
      there was no(callback).apply(any[String],any[String])
      dict.findSuggestion(" ") must_== None
      dict.findSuggestion("") must_== Some("first")
    }

    "remove extra spaced on the line" in {
      val toTrim = new ByteArrayInputStream("   first  \n  second  \n  sector   \n".getBytes())
      val dict = AutoCompleteDictionary.fromStream(toTrim,callback)
      there was no(callback).apply(any[String],any[String])
      dict.findSuggestion("fi") must_== Some("first")
      dict.findSuggestion("se") must_== Some("second")
    }

    "warn if compound term has same prefix" in {
      val toRead = new ByteArrayInputStream("  second  \n  second attack  \n".getBytes())
      val dict = AutoCompleteDictionary.fromStream(toRead,callback)
      there was one(callback).apply("second attack","Prefix 'second' already defined")
      dict.findSuggestion("se") must_== Some("second")
      dict.findSuggestion("second at") must_== Some("second attack")
    }

    "warn if compound term has same prefix in reverse" in {
      val toRead = new ByteArrayInputStream("  second attack \n  second  \n".getBytes())
      val dict = AutoCompleteDictionary.fromStream(toRead,callback)
      there was one(callback).apply("second","Prefix 'second' already defined")
      dict.findSuggestion("se") must_== Some("second attack")
    }

    "ignore comment lines" in {
      val toRead = new ByteArrayInputStream("# comment\n  second  \n".getBytes())
      val dict = AutoCompleteDictionary.fromStream(toRead,callback)
      dict.findSuggestion("# co") must_== None
    }
  }
}