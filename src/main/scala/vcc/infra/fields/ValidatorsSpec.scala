//$Id$
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

package vcc.infra.fields

import org.specs._
import org.specs.runner.JUnit4

class ValidatorTest extends JUnit4(ValidatorsSpec)

object ValidatorsSpec extends Specification {
  var intValidator:FieldValidator[Int]=null
  var stringValidator:FieldValidator[String]=null
  
  val baseIntContext = beforeContext {
    intValidator = new DefaultIntFieldValidator()
  }
  
  val boundedIntContext = beforeContext {
    intValidator = new DefaultIntFieldValidator(BoundedInteger(-2 to 4))
  } 

  val notNullIntContext = beforeContext {
    intValidator = new DefaultIntFieldValidator(Mandatory())
  } 
  
  val composedIntContext = beforeContext {
    intValidator = new DefaultIntFieldValidator(Mandatory(),BoundedInteger(-2 to 4))
  } 

  val notNullStringContext = beforeContext {
    stringValidator = new DefaultStringFieldValidator(Mandatory())
  } 
  "baseInt validator" ->-(baseIntContext) should {
    "provide invalid on invalid numbers" in {
      intValidator.validate("a").isValid must beFalse
      intValidator.validate(" ").isValid must beFalse
    }
    "provide undefined on a null " in {
      intValidator.validate(null).isDefined must beFalse
      intValidator.validate(null).isValid must beTrue
      intValidator.validate("").isDefined must beFalse
      intValidator.validate("").isValid must beTrue
    }
    "provide valids for numbers" in {
      for(v<-(-10 to 10)) {
        intValidator.validate(v.toString) must_== Defined(v)
      }
    }
  }
  
  "bounded integer validator" ->-(boundedIntContext) should {
    "only be valid for number in range" in {
      val rng = -2 to 4
      for(v<-(-10 to 10)) {
        val checked = intValidator.validate(v.toString) 
        if(rng.contains(v)) checked must_== Defined(v)
        else checked.isValid must beFalse
      } 
    }
    "accept undefined values" in {
      intValidator.validate(null) must_== Undefined
    }
  }

  "mandatory integer validator" ->-(notNullIntContext) should {
    "only accept defined values" in {
      intValidator.validate(null).isValid must beFalse
      intValidator.validate("1").isValid must beTrue
      intValidator.validate("1").isDefined must beTrue
    }
  }

  "composed integer validator" ->-(composedIntContext) should {
    "only accept defined values" in {
      intValidator.validate(null).isValid must beFalse
      val rng = -2 to 4
      for(v<-(-10 to 10)) {
        val checked = intValidator.validate(v.toString) 
        if(rng.contains(v)) checked must_== Defined(v)
        else checked.isValid must beFalse
      } 
    }
  }
  "mandatory string validator" ->-(notNullStringContext) should {
    "only accept defined values" in {
      stringValidator.validate(null).isValid must beFalse
      stringValidator.validate("a string").isValid must beTrue
    }
  }
}
