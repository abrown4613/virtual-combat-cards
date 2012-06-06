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

import org.specs2.SpecificationWithJUnit
import vcc.advtools.Monster._

class MonsterReaderTest extends SpecificationWithJUnit {

  def is = "MonsterReaderTest".title ^
    run(monster0()) ^
    run(monsterCustom0()) ^
    run(monster1()) ^
    end

  def run(monster: MonsterCase) = {
    "Handle " + monster.resource ^ monster.baseDefinition ^ endp
  }

  abstract class MonsterCase(val resource: String) {
    val reader = new MonsterReader(this.getClass.getClassLoader.getResourceAsStream("vcc/advtools/" + resource))
    val expectedName: String
    val expectedGroupTaxonomy: GroupTaxonomy
    val expectedBestiaryTaxonomy: BestiaryTaxonomy
    val expectedDefense: Defense
    val expectedSkills: Map[String, Int]
    val expectedAbilityScore: AbilityScores
    val expectedEquipment: Option[String] = None
    val expectedLanguages: Option[String] = None
    val expectedAlignment: String
    val expectedBaseStats: BaseStats
    val expectedCompendiumID: Option[Int] = None
    val expectedSenses: Option[String] = None
    val expectedSusceptibilities: List[Susceptibility] = Nil
    val expectedSpeeds: String
    val expectedPowers: List[Power]

    def baseDefinition = {
      "  has correct name" ! (reader.getName must_== expectedName) ^
        "has expected compendium id" ! (reader.getCompendiumID must_== expectedCompendiumID) ^
        "has correct group taxonomy" ! (reader.getGroupCategory must_== expectedGroupTaxonomy) ^
        "has correct bestiary taxonomy" ! (reader.getTaxonomy must_== expectedBestiaryTaxonomy) ^
        "has correct senses" ! (reader.getSenses must_== expectedSenses) ^
        "has correct defense" ! (reader.getDefense must_== expectedDefense) ^
        "has correct ability" ! (reader.getAbilityScores must_== expectedAbilityScore) ^
        "has correct skills" ! (reader.getSkills must_== expectedSkills) ^
        "has equipment" ! (reader.getEquipment must_== expectedEquipment) ^
        "has languages" ! (reader.getLanguages must_== expectedLanguages) ^
        "has alignment" ! (reader.getAlignment must_== expectedAlignment) ^
        "has correct speed" ! (reader.getSpeeds must_== expectedSpeeds) ^
        "has correct base stats" ! (reader.getBaseStats must_== expectedBaseStats) ^
        "has correct susceptibilities" ! (reader.getSusceptibilities must_== expectedSusceptibilities) ^
        "has correct powers" ! (reader.getPowers must_== expectedPowers)
    }
  }

  case class monster0() extends MonsterCase("monster-0.xml") {
    val expectedName = "El Monster 99999"
    val expectedGroupTaxonomy = GroupTaxonomy("Lurker", "Standard", false, 7, 300)
    val expectedBestiaryTaxonomy = BestiaryTaxonomy("Tiny", "Immortal", "Humanoid", Some("Devil, Aquatic"), None)
    val expectedDefense = Defense(21, 20, 19, 18)
    val expectedSkills = Map("Perception" -> 12, "Bluff" -> 9, "Stealth" -> 13)
    val expectedAbilityScore = AbilityScores(12, 15, 20, 14, 18, 12)
    val expectedAlignment = "Evil"
    override val expectedLanguages = Some("Common, Supernal")
    val expectedBaseStats = BaseStats(63, 12, 0, 0)
    override val expectedCompendiumID = Some(99999)
    override val expectedSenses = Some("Darkvision, tremorsense 10")
    override val expectedSusceptibilities: List[Susceptibility] = List(Resistance("Fire", 5))
    val expectedSpeeds = "Speed 4, Fly 7 (hover)"
    val expectedPowers = List(
      Power("Razor", "Standard", "At-Will", "Melee", true),
      Power("Tail Sting", "Standard", "At-Will", "Melee", true),
      Power("Vanish ", "Standard", "At-Will", "", false),
      Power("Quick Escape", "Immediate Reaction", "Encounter", "", false))
  }

  case class monster1() extends MonsterCase("monster-1.xml") {
    val expectedName = "Charlie Victor Lima"
    val expectedGroupTaxonomy = GroupTaxonomy("Skirmisher", "Elite", true, 11, 1200)
    val expectedBestiaryTaxonomy = BestiaryTaxonomy("Medium", "Natural", "Humanoid", Some("Undead"), None)
    val expectedDefense = Defense(28, 27, 26, 25)
    val expectedSkills = Map("Acrobatics" -> 15, "Thievery" -> 15, "Intimidate" -> 13, "Bluff" -> 13,
      "Perception" -> 8, "Athletics" -> 13, "Stealth" -> 15)
    val expectedAbilityScore = AbilityScores(16, 13, 20, 12, 11, 16)
    val expectedAlignment = "Evil"
    override val expectedLanguages = Some("Common")
    val expectedBaseStats = BaseStats(248, 12, 1, 2)
    override val expectedCompendiumID = Some(98765)
    override val expectedSenses = Some("Darkvision")
    override val expectedEquipment = Some("Leather Armor, Short sword")
    override val expectedSusceptibilities = List(Resistance("Necrotic", 10), Vulnerability("Radiant", 10),
      Immune("Disease"), Immune("Poison"))
    val expectedSpeeds = "Speed 8, Climb 4"
    val expectedPowers = List(
      Power("Short Sword", "Standard", "At-Will", "Melee", true),
      Power("Deft Strick", "Standard", "At-Will", "Melee", false),
      Power("Imperiling Strike", "Standard", "Encounter", "Melee", false),
      Power("Blood Drain", "Standard", "Recharge when an adjacent creature becomes bloodied", "Melee", false),
      Power("Dominating Gaze", "Minor", "Recharge 4", "Ranged", false),
      Power("Mist Form", "Standard", "Encounter", "", false),
      Power("Second Wind", "Standard", "Encounter", "", false))
  }

  case class monsterCustom0() extends MonsterCase("monster-custom0.xml") {
    val expectedName = "El cabron"
    val expectedGroupTaxonomy = GroupTaxonomy("Soldier", "Elite", false, 4, 350)
    val expectedBestiaryTaxonomy = BestiaryTaxonomy("Medium", "Fey", "Humanoid", None, Some("Drow"))
    val expectedDefense = Defense(23, 16, 19, 15)
    val expectedSkills = Map("Diplomacy" -> 3, "Nature" -> 3, "Streetwise" -> 3, "Acrobatics" -> 8, "Endurance" -> 3,
      "Heal" -> 3, "Insight" -> 3, "Religion" -> 4, "Thievery" -> 3, "Intimidate" -> 3, "History" -> 9,
      "Arcana" -> 4, "Bluff" -> 3, "Perception" -> 8, "Dungeoneering" -> 3, "Athletics" -> 4, "Stealth" -> 3)
    val expectedAbilityScore = AbilityScores(15, 12, 12, 15, 12, 12)
    override val expectedEquipment = Some("Longbow, Short sword, Scale Armor, Arrows (30)")
    override val expectedLanguages = Some("Goblin")
    val expectedAlignment = "Unaligned"
    val expectedBaseStats = BaseStats(120, 5, 1, 2)
    val expectedSpeeds = "Speed 6"
    val expectedPowers = List(
      Power("Whip Ash", "Standard", "At-Will", "Melee", true),
      Power("New Utility Power", "Minor", "", "None", false))
  }

}