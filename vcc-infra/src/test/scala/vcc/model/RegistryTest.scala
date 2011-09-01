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
package vcc.model

import org.specs2.mutable.SpecificationWithJUnit

class RegistryTest extends SpecificationWithJUnit {
  val aRegistry = new Registry[String]()

  "aRegistry" should {
    "register an object" in {
      aRegistry.register[Int]("some", 10)
      aRegistry.contains("some") must beTrue
    }

    "get Some if manifest matches" in {
      aRegistry.register[Int]("some", 10)
      val i = aRegistry.get[Int]("some")
      i.isDefined must beTrue
      i.get must_== 10
    }

    "get None object if manifest matches" in {
      aRegistry.register[Int]("some", 10)
      val i = aRegistry.get[String]("some")
      i.isDefined must beFalse
    }
  }

  "The Registry" should {
    "get Some if manifest matches" in {
      Registry.register[Int]("some", 10)
      val i = Registry.get[Int]("some")
      i.isDefined must beTrue
      i.get must_== 10
    }

    "get None object if manifest matches" in {
      Registry.register[Int]("some", 10)
      val i = Registry.get[String]("some")
      i.isDefined must beFalse
    }
  }

  "experimental" should {
    "register base on class" in {
      val reg = new Registry[Class[_]]()
      reg.register[Int](classOf[Int], 10)
      val o = reg.get[Int](classOf[Int])
      o.isDefined must beTrue
      o.get must_== 10
    }
  }
}