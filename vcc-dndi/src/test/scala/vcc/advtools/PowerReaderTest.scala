/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.advtools

import org.specs2.mutable.SpecificationWithJUnit
import vcc.advtools.Monster._
import xml.XML
import vcc.dndi.reader.{AtWillUsage}

class PowerReaderTest extends SpecificationWithJUnit {


  "read simple power" in {
    readPower("power-simple.xml") must_==
      Power("Short Sword", "Standard", AtWillUsage(0), BasicAttack("Melee"), None, Set("Weapon"),
        Attack(List(AttackBonus("AC", 13)), Some("1d6+8"), Some("damage")))
  }

  "read trigger" in {
    readPower("power-trigger-noaction.xml") must_==
      Power("Random Eye Ray", "No Action", AtWillUsage(0), NormalAttack("Ranged"), Some("The trigger"), Set(),
        Attack(List(), None, None,
          AttackResult(List(), None, None),
          AttackResult(List(), None, None),
          AttackResult(List(), None, Some("The beholder uses one random eye ray against the triggering enemy."))
        ))
  }

  def readPower(file: String): Power = {
    val xml = XML.load(this.getClass.getClassLoader.getResourceAsStream("vcc/advtools/" + file))
    new PowerReader(xml).read()
  }
}