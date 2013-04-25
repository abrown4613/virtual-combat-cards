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
package vcc.infra.fields

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class ValidatorsTest extends SpecificationWithJUnit {

  trait baseIntContext extends Scope {
    val intValidator = new DefaultIntFieldValidator()
  }

  trait boundedIntContext extends Scope {
    val intValidator = new DefaultIntFieldValidator(BoundedInteger(-2 to 4))
  }

  trait notNullIntContext extends Scope {
    val intValidator = new DefaultIntFieldValidator(Mandatory())
  }

  trait composedIntContext extends Scope {
    val intValidator = new DefaultIntFieldValidator(Mandatory(), BoundedInteger(-2 to 4))
  }

  trait notNullStringContext extends Scope {
    val stringValidator = new DefaultStringFieldValidator(Mandatory())
  }

  trait defaultIntContext extends Scope {
    val DefaultValue = 123
    val intValidator = new IntFieldWithDefaultValidator(DefaultValue, Mandatory())
  }

  "baseInt validator" should {
    "provide invalid on invalid numbers" in new baseIntContext {
      intValidator.validate("a").isValid must beFalse
      intValidator.validate(" ").isValid must beFalse
    }
    "provide undefined on a null " in new baseIntContext {
      intValidator.validate(null).isDefined must beFalse
      intValidator.validate(null).isValid must beTrue
      intValidator.validate("").isDefined must beFalse
      intValidator.validate("").isValid must beTrue
    }
    "provide valid for numbers" in new baseIntContext {
      for (v <- (-10 to 10)) {
        intValidator.validate(v.toString) must_== Defined(v)
      }
    }
  }

  "bounded integer validator" should {
    "only be valid for number in range" in new boundedIntContext {
      val rng = -2 to 4
      for (v <- (-10 to 10)) {
        val checked = intValidator.validate(v.toString)
        if (rng.contains(v)) checked must_== Defined(v)
        else checked.isValid must beFalse
      }
    }
    "accept undefined values" in new boundedIntContext {
      intValidator.validate(null) must_== Undefined
    }
  }

  "mandatory integer validator" should {
    "only accept defined values" in new notNullIntContext {
      intValidator.validate(null).isValid must beFalse
      intValidator.validate("1").isValid must beTrue
      intValidator.validate("1").isDefined must beTrue
    }
  }

  "composed integer validator" should {
    "only accept defined values" in new composedIntContext {
      intValidator.validate(null).isValid must beFalse
      val rng = -2 to 4
      for (v <- (-10 to 10)) {
        val checked = intValidator.validate(v.toString)
        if (rng.contains(v)) checked must_== Defined(v)
        else checked.isValid must beFalse
      }
    }
  }
  "mandatory string validator" should {
    "only accept defined values" in new notNullStringContext {
      stringValidator.validate(null).isValid must beFalse
      stringValidator.validate("a string").isValid must beTrue
    }
  }

  "int validator with default" should {
    "accept valid int or provide default" in new defaultIntContext {
      intValidator.validate(null) must_== Defined(DefaultValue)
      intValidator.validate("some") must_== Defined(DefaultValue)
      intValidator.validate("1") must_== Defined(1)
      intValidator.validate("12") must_== Defined(12)
    }
  }
}