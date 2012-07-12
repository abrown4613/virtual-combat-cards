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
    run(monster1()) ^
    run(monsterCustom0()) ^
    "Provide a digest for the content" ! digestTest ^
    end

  def digestTest = {
    val reader = new MonsterReader(this.getClass.getClassLoader.getResourceAsStream("vcc/advtools/" + "monster-0.xml"))
    reader.getContentDigest must_== "5038c513-dfab-3606-9bcf-9e2d21f07d54"
  }

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
    val expectedTraits: List[BaseCreatureTrait]

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
        "has correct powers" ! (reader.getPowers must_== expectedPowers) ^
        "have correct traits" ! (reader.getCreatureTraits must_== expectedTraits)
    }
  }

  case class monster0() extends MonsterCase("monster-0.xml") {
    val expectedName = "El Monster 99999"
    val expectedGroupTaxonomy = GroupTaxonomy("Lurker", "Standard", false, 7, 300)
    val expectedBestiaryTaxonomy = BestiaryTaxonomy("Tiny", "Immortal", "Humanoid", Some("Devil, Aquatic"), None)
    val expectedDefense = Defense(21, 20, 19, 18)
    val expectedSkills = Map("Perception" -> 12, "Bluff" -> 9, "Stealth" -> 13)
    val expectedAbilityScore = AbilityScores(12, 20, 15, 14, 18, 12)
    val expectedAlignment = "Evil"
    override val expectedLanguages = Some("Common, Supernal")
    val expectedBaseStats = BaseStats(63, 12, 0, 0)
    override val expectedCompendiumID = Some(99999)
    override val expectedSenses = Some("Darkvision, tremorsense 10")
    override val expectedSusceptibilities: List[Susceptibility] = List(Resistance("Fire", 5))
    val expectedSpeeds = "4, Fly 7 (hover)"
    val expectedPowers = List(
      Power("Razor", "Standard", "At-Will", BasicAttack("Melee"), Set(),
        Attack(List(AttackBonus("AC", 12)), Some("1d4 + 4"), Some("damage"))),
      Power("Tail Sting", "Standard", "At-Will", BasicAttack("Melee"), Set("Poison"),
        Attack(List(AttackBonus("AC", 12)),
          AttackResult(List(Attack(List(AttackBonus("Fortitude", 10)), None, Some("the target takes ongoing 5 poison damage and is slowed (save ends both)."))), Some("1d8 + 4"), Some("damage, and the imp makes a secondary attack against the same target")),
          AttackResult(List(), None, None),
          AttackResult(List(), None, None)
        )),
      Power("Vanish ", "Standard", "At-Will", NonAttack, Set("Illusion"),
        makeUtilityAttack("3d6 + 4", "The imp becomes invisible until the end of its next turn or until it attacks.")),
      Power("Quick Escape", "Immediate Reaction", "Encounter", NonAttack, Set(),
        makeUtilityAttack("3d6 + 4", "The imp uses vanish as an immediate reaction.")))
    val expectedTraits = List(CreatureTrait("Bleed the Helpless", "When the assassin imp attacks a sleeping or helpless target, its razor attack deals +2d6 damage and ongoing 5 damage (save ends)."))
  }

  case class monster1() extends MonsterCase("monster-1.xml") {
    val expectedName = "Charlie Victor Lima"
    val expectedGroupTaxonomy = GroupTaxonomy("Skirmisher", "Elite", true, 11, 1200)
    val expectedBestiaryTaxonomy = BestiaryTaxonomy("Medium", "Natural", "Humanoid", Some("Undead"), None)
    val expectedDefense = Defense(28, 27, 26, 25)
    val expectedSkills = Map("Acrobatics" -> 15, "Thievery" -> 15, "Intimidate" -> 13, "Bluff" -> 13,
      "Perception" -> 8, "Athletics" -> 13, "Stealth" -> 15)
    val expectedAbilityScore = AbilityScores(16, 20, 13, 12, 11, 16)
    val expectedAlignment = "Evil"
    override val expectedLanguages = Some("Common")
    val expectedBaseStats = BaseStats(248, 12, 1, 2)
    override val expectedCompendiumID = Some(98765)
    override val expectedSenses = Some("Darkvision")
    override val expectedEquipment = Some("Leather Armor, Short sword")
    override val expectedSusceptibilities = List(Resistance("Necrotic", 10), Vulnerability("Radiant", 10),
      Immune("Disease"), Immune("Poison"))
    val expectedSpeeds = "8, Climb 4"
    val expectedPowers = List(
      Power("Short Sword", "Standard", "At-Will", BasicAttack("Melee"), Set("Weapon"),
        Attack(List(AttackBonus("AC", 13)), Some("1d6+8"), Some("damage"))),
      Power("Deft Strick", "Standard", "At-Will", NormalAttack("Melee"), Set("Weapon"),
        Attack(List(AttackBonus("AC", 15)), Some("1d6+10"), Some("damage"))),
      Power("Imperiling Strike", "Standard", "Encounter", NormalAttack("Melee"), Set(),
        Attack(List(AttackBonus("Fortitude", 15)), Some("1d6+10"), Some("damage, and the target takes a -3 penalty to AC and Reflex defenses until the end of Dude’s next turn"))),
      Power("Blood Drain", "Standard", "Recharge when an adjacent creature becomes bloodied", NormalAttack("Melee"), Set("Healing"),
        Attack(List(AttackBonus("Will", 13)), Some("2d12+8"), Some("damage, the target is weakened (save ends), and Dude regains 46 hit points"))),
      Power("Dominating Gaze", "Minor", "Recharge 4", NormalAttack("Ranged"), Set("Charm"),
        Attack(List(AttackBonus("Will", 13)), None, Some("the target is dominated (save ends, with a -2 penalty on the saving throw)."))),
      Power("Mist Form", "Standard", "Encounter", NonAttack, Set("Polymorph"),
        makeUtilityAttack("4d6 + 5", "Dude becomes insubstantial and gains a fl y speed of 12, but cannot make attacks. Dude can remain in mist form for up to 1 hour or end the effect as a minor action.")),
      Power("Second Wind", "Standard", "Encounter", NonAttack, Set("Healing"),
        makeUtilityAttack("4d6 + 5", "Dude spends a healing surge and regains 46 hit points. She gains a +2 bonus to all defenses until the start of her next turn.")))
    val expectedTraits = List(
      Aura("Sepulchral Stench", 3, "enemies in the aura take a -2 penalty to all defenses."),
      CreatureTrait("Combat Advantage", "Dude deals an extra 3d6 damage with her attacks against any target she has combat advantage against."))
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
    val expectedSpeeds = "6"
    val expectedPowers = List(
      Power("Whip Ash", "Standard", "At-Will", BasicAttack("Melee"), Set(),
        Attack(List(AttackBonus("AC", 9)), Some("1d10 + 7"), Some("damage."))),
      Power("New Utility Power", "Minor", "", NonAttack, Set(),
        makeUtilityAttack("1d10 + 7", "Enter Power Effect Here"))
    )
    val expectedTraits = List(
      CreatureTrait("Goblin coolnes", "Shift for free when hit"),
      Aura("Aura of Pain", 1, "-2 to all Attackers"))
  }

  private def makeUtilityAttack(damage: String, effect: String) =
    Attack(
      List(),
      AttackResult(List(), Some(damage), None),
      AttackResult(List(), None, None),
      AttackResult(List(), None, Some(effect)))
}