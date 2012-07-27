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
import vcc.dndi.reader.{RechargeDiceUsage, AtWillUsage}

class PowerReaderTest extends SpecificationWithJUnit {

  "read simple power" in {
    readPower("power-simple.xml") must_==
      Power("Short Sword", "Standard", AtWillUsage(0), BasicAttack("Melee"), Set("Weapon"),
        b("_Attack_: +13 vs. AC\n_Hit_: 1d6+8 damage"))
  }

  "read trigger" in {
    readPower("power-trigger-noaction.xml") must_==
      Power("Random Eye Ray", "No Action", AtWillUsage(0), NormalAttack("Ranged"), Set(),
        b("_Trigger: _The trigger\n" +
          "_Effect (No Action): _The beholder uses one random eye ray against the triggering enemy."))
  }

  "read power with range constrained and no damage" in {
    readPower("power-ranged-constrained-no-damage.xml") must_==
      Power("Luring Glare", "Minor", AtWillUsage(0), NormalAttack("Close Blast"), Set("Charm"),
        b("_Attack_: Close Blast 10 (one creature in the blast); +22 vs. Will\n" +
          "_Hit_: The dragon slides the target up to 3 squares."))
  }

  "read power with simple effect and requirement" in {
    readPower("power-simple-effect-requirement.xml") must_==
      Power("Eye Ray Frenzy", "Standard", RechargeDiceUsage(6), NormalAttack("Ranged"), Set(),
        b("_Requirement_: The beholder must be bloodied\n" +
          "_Effect: _As eye rays above, except the beholder makes three eye ray attacks."))
  }

  "read power with miss no damage" in {
    readPower("power-with-miss-no-damage.xml") must_==
      Power("Bite", "Standard", AtWillUsage(0), BasicAttack("Melee"), Set("Poison"),
        b("_Attack_: Melee 3 (one creature); +24 vs. AC\n" +
          "_Hit_: 3d10 + 14 damage, and ongoing 10 poison damage (save ends).\n" +
          "_Miss_: 10 poison damage."))
  }

  "read power with attack detail and miss" in {
    readPower("power-attack-description.xml") must_==
      Power("Claw", "Standard", AtWillUsage(0), NormalAttack("Melee"), Set(),
        b("_Attack_: Melee 3 (one or two creatures); +24 vs. AC. Some description.\n" +
          "_Hit_: 3d8 + 13 damage, and the dragon shifts up to 2 squares.\n" +
          "_Miss_: 1d6+2 and target pushed 1 square."))
  }

  "read power with after effect" in {
    readPower("power-with-after-effect.xml") must_==
      Power("Breath Weapon", "Standard", RechargeDiceUsage(5), NormalAttack("close blast"), Set("Poison"),
        b("_Attack_: close blast 5 (enemies in the blast); +22 vs. Fortitude\n" +
          "_Hit_: 2d12 + 12 poison damage, and the target is slowed and takes ongoing 15 poison damage (save ends both).\n" +
          "\t_Aftereffects_: The target is slowed (save ends)."
        ))
  }

  private def b(text: String) = FormattedTextParser.parseBlock(text).get

  private def readPower(file: String): Power = {
    val xml = XML.load(this.getClass.getClassLoader.getResourceAsStream("vcc/advtools/" + file))
    new PowerReader(xml).read()
  }
}