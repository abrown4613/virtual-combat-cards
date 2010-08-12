/**
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
//$Id$

/*
 * This code is based on Bjarte S. Karlsen  work on outsidertools project.
 *
 * Original code available here:
 * http://github.com/bjartek/outsidertools/blob/master/src/main/scala/org/bjartek/outsidertools/importer.scala
 *
 */
package vcc.domain.dndi

import scala.xml._

object CharacterBuilderObject {
  case class Rules(val typ: String, val value: String, val url: Option[String])

  case class PowerWeapon(val name: String, val hit: String, val dmg: String, val stat: String, val defense: String, val hitSummary: String, val dmgSummary: String, val cond: String);

  case class Power(val name: String, val url: Option[String], val usage: String, val action: String, val weapons: Map[String, PowerWeapon])

  case class MagicEnchantment(name: String, refUrl: String)
  case class Equipment(kind: String, name: String, count: Int, ecount: Int, refUrl: String, enchantment: Option[MagicEnchantment])
  case class Skill(var name: String, var score: Int, var url: Option[String])
  case class Stat(var name: String, var score: Int, var mod: Int)

  object CharacterObject {
    val statOrder = "Strength" :: "Constitution" :: "Dexterity" :: "Intelligence" :: "Wisdom" :: "Charisma" :: Nil
    val skillOrder = "Athletics" :: "Endurance" :: "Acrobatics" :: "Stealth" :: "Thievery" :: "Arcana" :: "History" :: "Religion" :: "Dungeoneering" :: "Heal" :: "Insight" :: "Nature" :: "Perception" :: "Bluff" :: "Diplomacy" :: "Intimidate" :: "Streetwise" :: Nil
    val statSkills = ("Strength" -> List("Athletics")) :: ("Constitution" -> List("Endurance")) :: ("Dexterity" -> List("Acrobatics", "Stealth", "Thievery")) :: ("Intelligence" -> List("Arcana", "History", "Religion")) :: ("Wisdom" -> List("Dungeoneering", "Heal", "Insight", "Nature", "Perception")) :: ("Charisma" -> List("Bluff", "Diplomacy", "Intimidate", "Streetwise")) :: Nil
  }

  def extractStat(stat: Node): Seq[(String, Int)] = {
    val value = (stat \ "@value").text.toInt
    (stat \\ "alias").map(x => (x \ "@name").text.toLowerCase -> value)
  }
}

class CharacterBuilderObject(val char: Elem) {
  import CharacterBuilderObject._

  val sheet = char \ "CharacterSheet";

  lazy val stats = Map() ++ (sheet \\ "Stat").flatMap(extractStat(_))

  lazy val tally = for{
    tally <- sheet \ "RulesElementTally" \ "RulesElement"
    tt <- tally \ "@type"
    tn <- tally \ "@name"
  } yield {
      val url = tally.attribute("url") match {case Some(node: Node) => Some(node.text) case None => None}
      Rules(tt.text, tn.text, url);
    }

  def findPower(name: String) = tally.filter(_.value == name).toList.head

  def filterTally(typ: String) = tally.filter(_.typ == typ).map(_.value)

  lazy val details = sheet \ "Details"

  def detail(key: String) = (details \ key).text.trim

  lazy val race = tally.filter(_.typ == "Race").toList.head

  lazy val clazz = tally.filter(_.typ == "Class").toList.head

  lazy val power = Map() ++ (for{
    power <- sheet \\ "Power"
    name <- power \ "@name"
  } yield {
      val specifics = Map() ++ (for{
        specific <- power \ "specific"
        sn <- specific \ "@name"
      } yield {
          sn.text.trim -> specific.text.trim
        })
      val weapons = Map() ++ (for{
        weapon <- power \ "Weapon"
        wn <- weapon \ "@name"
        hit <- weapon \ "AttackBonus"
        dmg <- weapon \ "Damage"
        stat <- weapon \ "AttackStat"
        defense <- weapon \ "Defense"
        hitSummary <- weapon \ "HitComponents"
        dmgSummary <- weapon \ "DamageComponents"
      } yield {
          val cond = weapon \ "Conditions"
          wn.text -> PowerWeapon(wn.text.trim, hit.text.trim, dmg.text.trim, stat.text.trim, defense.text.trim, hitSummary.text.trim, dmgSummary.text.trim, cond.text.trim);
        });
      name.text -> Power(name.text, findPower(name.text).url, specifics("Power Usage"), specifics("Action Type"), weapons)
    })

  lazy val equipment = Map() ++ (for{
    loot <- sheet \\ "loot";
    le <- loot \ "@equip-count";
    lc <- loot \ "@count" if lc.text.toInt != 0
  } yield {
      val name = loot \\ "@name"
      val typ = loot \\ "@type"
      val url = loot \\ "@url"

      val lu = url.toList.head
      val lt = typ.toList.head
      val ln = name.toList.head

      lt.text match {
        case "Weapon" => {
          if (typ.contains("Magic Item")) {
            ln.text -> Equipment("Weapon", ln.text, lc.text.toInt, le.text.toInt, lu.text, Some(MagicEnchantment(name(1).text, url(1).text)))
          } else {
            ln.text -> Equipment("Weapon", ln.text, lc.text.toInt, le.text.toInt, lu.text, None)
          }
        }
        case "Armor" => {
          if (typ.contains("Magic Item")) {
            ln.text -> Equipment("Armor", ln.text, lc.text.toInt, le.text.toInt, lu.text, Some(MagicEnchantment(name(1).text, url(1).text)))
          } else {
            ln.text -> Equipment("Armor", ln.text, lc.text.toInt, le.text.toInt, lu.text, None)
          }
        }
        case "Ritual" => ln.text -> Equipment("Ritual", ln.text, lc.text.toInt, le.text.toInt, lu.text, None)

        case _ => ln.text -> Equipment(lt.text, ln.text, lc.text.toInt, le.text.toInt, lu.text, None)
      }
    })

  lazy val classFeature = filterTally("Class Feature").toList
  lazy val racialTrait = filterTally("Racial Trait").toList
  lazy val feats = Map() ++ (tally.filter(_.typ == "Feat").map(kv => kv.value -> kv.url));
  lazy val skill = Map() ++ (tally.filter(_.typ == "Skill").map(kv => kv.value -> Skill(kv.value, stats(kv.value.toLowerCase), kv.url)))
  lazy val stat = Map() ++ (CharacterObject.statOrder.map(name => name -> Stat(name, stats(name.toLowerCase), stats(name.toLowerCase + " modifier"))))


  def getDatum(): Map[String, String] = {
    Map(
      "classid" -> "vcc-class:character",
      "base:class" -> clazz.value,
      "base:level" -> detail("Level"),
      "base:name" -> detail("name"),
      "base:race" -> race.value,
      "skill:insight" -> skill("Insight").score.toString,
      "skill:perception" -> skill("Perception").score.toString,
      "stat:ac" -> stats("ac").toString,
      "stat:fortitude" -> stats("fortitude defense").toString,
      "stat:hp" -> stats("hit points").toString,
      "stat:initiative" -> stats("initiative").toString,
      "stat:reflex" -> stats("reflex defense").toString,
      "stat:will" -> stats("will defense").toString,
      "base:senses" -> filterTally("Vision").mkString(", "))
  }
}