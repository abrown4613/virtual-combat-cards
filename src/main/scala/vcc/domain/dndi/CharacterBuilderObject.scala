//$Id$

/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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

/*
 * This code is based on Bjarte S. Karlsen  work on outsidertools project.
 *
 * Original code available here:
 * http://github.com/bjartek/outsidertools/blob/master/src/main/scala/org/bjartek/outsidertools/importer.scala
 *
 */
package vcc.domain.dndi

import scala.xml._

//import vcc.domain.dndi.CharacterBuilderObject._ ;
//CharacterXmlImporter(scala.xml.XML.loadFile("C:/temp/vcc/char/Fionn.dnd4e"))

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

}

class CharacterBuilderObject(val char: Elem) {
  import CharacterBuilderObject._

  lazy val stats = {
    Map() ++ (for{
      stat <- sheet \\ "Stat"
      value <- stat \ "@value"
      name <- stat \ "@name"
    } yield {
        name.text.toLowerCase -> value.text.toInt
      })
  }

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

  val sheet = char \ "CharacterSheet";

  lazy val details = sheet \ "Details"

  def detail(key: String) = (details \ key).text.trim

  lazy val race = tally.filter(_.typ == "Race").toList.head
  lazy val claz = tally.filter(_.typ == "Class").toList.head

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


  def getDatum():Map[String,String] = {
    Map(
      "classid" -> "vcc-class:character",
      "base:class"-> claz.value,
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
  /*
    def generate() : Character  = {

      val c = Character();
      c.name = detail("name")
      c.level = detail("Level").toInt
      c.player = detail("Player")
      c.deity = detail("Deity")
      c.alignment = filterTally("Alignment").toList match { case List(element) => element; case _ => "" }
      c.size = filterTally("Size").toList.head
      c.languages = filterTally("Language").toList
      c.race = race.value
      c.raceUrl = race.url
      c.height = detail("Height")
      c.weight = detail("Weight")
      c.gender = detail("Gender")
      c.age = detail("Age")
      c.money = detail("CarriedMoney")
      c.bank = detail("StoredMoney")
      c.traits = detail("Traits")

      if(detail("Companions") != "") {
          c.companions = detail("Companions")
        }

      if(detail("Appearance") != "") {
          c.appearance = detail("Appearance")
        }


      if(detail("Experience") != "")  {
        c.xp = detail("Experience").toInt
       }

      c.hp = stats("hit points").toInt
      c.surges = stats("healing surges").toInt
      c.init = stats("initiative").toInt
      c.speed = stats("speed").toInt
      c.ac = stats("ac").toInt
      c.fort = stats("fortitude defense").toInt
      c.ref = stats("reflex defense").toInt
      c.will = stats("will defense").toInt
      c.clazz = claz.value
      c.clazzUrl = claz.url
      c.passivePerception = stats("passive perception")
      c.passiveInsight = stats("passive insight")


     c
    }
  */
}