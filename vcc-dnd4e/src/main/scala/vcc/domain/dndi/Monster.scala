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
package vcc.domain.dndi

import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq, Text => XmlText, Elem}
import vcc.domain.dndi.Monster.{Aura, PowerDescriptionSupplement}
import vcc.infra.xtemplate.TemplateDataSource

object Monster {

  //Construction elements
  case class Aura(name: String, desc: String) extends StatBlockDataSource {
    def extract(key: String): Option[String] = {
      val s: String = key.toLowerCase match {
        case "name" => name
        case "description" => desc
        case _ => null
      }
      if (s != null) Some(s) else None
    }

    def extractGroup(dontcare: String) = Nil
  }

  case class PowerDescriptionSupplement(emphasis: String, text: String) extends StatBlockDataSource {
    def extract(key: String): Option[String] = {
      val s: String = key.toLowerCase match {
        case "header" => emphasis
        case "description" => text
        case _ => null
      }
      if (s != null) Some(s) else None
    }

    def extractGroup(dontcare: String) = Nil

  }

  case class Power(icon: Seq[Parser.IconType.Value], name: String, action: String, keywords: String) extends StatBlockDataSource {
    private var _supplemnt: List[PowerDescriptionSupplement] = Nil

    def extract(key: String): Option[String] = {
      val s: String = key.toLowerCase match {
        case "name" => name
        case "description" => description
        case "action" => action
        case "type" if (icon != null && !icon.isEmpty) =>
          icon.map(i => Parser.IconType.iconToImage(i)).mkString(";")
        case "keywords" => keywords
        case _ => null
      }
      if (s != null) Some(s) else None
    }

    def extractGroup(key: String) = if (key.toLowerCase == "power description supplement") this._supplemnt.toSeq else Nil

    var description: String = null

    def addDescriptionSupplement(sup: PowerDescriptionSupplement) {
      _supplemnt = _supplemnt ::: List(sup)
    }

    override def toString: String = "Power(" + icon + ", " + name + ", " + action + ", " + keywords + ", " + description + ", " + this._supplemnt + ")"

    def supplement = _supplemnt
  }

}

/**
 * A monster imported from DNDI.
 * @param id DNDI ID value
 * @param legacyPowers list of power that where not in their own power sections
 * @param powersByAction Powers that are in sections, this will include Auras moved from previous format
 */
class MonsterNew(val id: Int,
                 protected var attributes: Map[String, String],
                 val legacyPowers: List[Power],
                 val powersByAction: Map[ActionType.Value, List[Power]]
        ) extends DNDIObject with TemplateDataSource {
  final val clazz = "monster"

  def templateGroup(key: String): List[TemplateDataSource] = key match {
    case "legacy" => legacyPowers
    case ActionType(at) => powersByAction.getOrElse(at,Nil)
    case _ => Nil
  }

  // No inlines yet
  def templateInlineXML(key: String): NodeSeq = Nil


  def templateVariable(key: String): Option[String] = this(key)
}

/**
 * Base monster load.
 */
class Monster(val id: Int) extends DNDIObject with StatBlockDataSource {

  final val clazz = "monster"

  var attributes = Map.empty[String,String]
  
  def extract(key: String): Option[String] = this(key.toLowerCase)

  def extractGroup(group: String) = group.toLowerCase match {
    case "powers" => this.powers
    case "auras" => this.auras
  }

  import vcc.domain.dndi.Parser._

  private var _auras: List[Monster.Aura] = Nil
  private var _power: List[Monster.Power] = Nil

  def powers = _power

  def auras = _auras

  private var _isMM3Format = false

  def isMM3Format = _isMM3Format

  def setMM3Format() {_isMM3Format = true}

  private[dndi] def set(attribute: String, value: String) {
    // Normalize some cases here:
    val normAttr = attribute.toLowerCase
    val normValue: String = normAttr match {
      case "hp" if (value.startsWith("1;")) => "1"
      case "role" if (value == "Minion") => "No Role"
      case _ => value
    }
    attributes = attributes+ (normAttr -> normValue)
  }

  private[dndi] def addAura(name: String, desc: String): Monster.Aura = {
    val a = new Monster.Aura(name, desc)
    _auras = _auras ::: List(a)
    a
  }

  protected[dndi] def addPower(icons: Seq[Parser.IconType.Value], name: String, action: String, keywords: String): Monster.Power = {
    val p = Monster.Power(icons, name, action, keywords)
    _power = _power ::: List(p)
    p
  }

  override def toString(): String = {
    "Monster[" + id + "](" + attributes + "; Aura=" + _auras + "; powers=" + _power + ")"
  }
}

import Parser._

class MonsterBuilder(monster: Monster) extends BlockReader {
  final val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")
  final val reSecondary = """^Secondary Attack\s*(.*)\s*$""".r
  final val primaryStats = Set("Initiative", "Senses", "HP", "Bloodied",
    "AC", "Fortitude", "Reflex", "Will",
    "Immune", "Resist", "Vulnerable",
    "Saving Throws", "Speed", "Action Points")

  private var _lastPower: Monster.Power = null

  private var _powerSupplement: String = null

  def getObject: DNDIObject = monster

  /**
   * Extract xp, level and role from compose text of the header
   */
  private def normalizeTitle(l: List[(String, String)]): List[(String, String)] = {
    l match {
      case ("xp", this.reXP(xp)) :: rest => ("xp", xp) :: normalizeTitle(rest)
      case ("level", this.reLevel(lvl, role)) :: rest => ("level", lvl) :: ("role", role) :: normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }

  /**
   * Add key value pairs into the _map variable. Key will be turned into uppercase
   */
  private def addToMap(items: List[(String, String)]) {
    for ((k, v) <- items) {monster.set(k, v)}
  }

  /**
   * Primary block includes auras, that become pais, need to pull them out
   */
  private def processPrimaryBlock(pairs: List[(String, String)]) {
    for ((k, v) <- pairs) {
      if (primaryStats(k)) monster.set(k, v)
      else monster.addAura(k, v)
    }
  }



  object PowerHeader {

    private val whitespace = """^[\s\n\r\t]*$""".r

    def unapply(parts: List[Part]): Option[(List[IconType.Value], String, String, String)] = {
      var p = breakToSpace(parts).filter(part=> part match {
        case Text(this.whitespace()) => false
        case _ => true
      })
      var icons: List[IconType.Value] = Nil

      while (!p.isEmpty && p.head.isInstanceOf[Icon]) {
        icons = p.head.asInstanceOf[Icon].itype :: icons
        p = p.tail
      }

      val name: String = p match {
        case Key(text) :: rest if (text != "Equipment") => // Need this check to avoid skipping equipments
          text
        case _ => return None
      }
      p = p.tail
      val action: String = p match {
        case Text(text) :: rest =>
          p = p.tail //Advance for last part
          text.trim()
        case _ => null
      }
      val keywords: String = p match {
        case Icon(IconType.Separator) :: Key(text) :: Nil => text
        case Icon(IconType.Separator) :: Key(text) :: Text(range) :: Nil =>
          //TODO Handle MM3 auras
          println("keywords (AURA): " + p)
          null
        case Icon(IconType.Separator) :: Text(trigger) :: Nil =>
          //TODO pwer with trigger
          println("keywords (trigger): " + trigger)
          null
        case Key("") :: Nil =>
          //TODO Handle Iconless MM3 traits.
          null
        case Nil => null
        case s =>
          println("**** Rest: " + s)
          return None
      }
      Some((icons, name, action, keywords))
    }
  }

  private def trimParts(parts: List[Part]): List[Part] = parts.map(p => p.transform(Parser.TrimProcess))

  def processBlock(block: BlockElement): Boolean = {
    block match {
      case HeaderBlock("H1#monster", pairs) => addToMap(normalizeTitle(pairs))
      case Block("P#flavor", parts@Key("Initiative") :: _) =>
        processPrimaryBlock(partsToPairs(trimParts(parts)))

      case Block("P#flavor", parts@Key("Alignment") :: _) =>
        addToMap(partsToPairs(trimParts(parts)))

      case Block("P#flavor alt", parts) =>
        parts match {
          case PowerHeader(icons, name, action, keywords) =>
            _lastPower = monster.addPower(icons, name, action, keywords)

          case Key("Equipment") :: SingleTextBreakToSpace(text) =>
            monster.set("Equipment", text)

          case Key("Alignment") :: rest =>
            addToMap(partsToPairs(parts))

          //Blocks with Skills and Str or just Str
          case list if (list contains Key("Str")) =>
            addToMap(partsToPairs(parts))

          case s => throw new Exception("Failed to process!! " + s)
          return false
        }
      case Block("P#flavorIndent", SingleTextBreakToNewLine(text)) if (_lastPower != null) =>
        // This is a power description 
        if (_powerSupplement != null) {
          _lastPower.addDescriptionSupplement(PowerDescriptionSupplement(_powerSupplement, text))
          _powerSupplement = null
        } else {
          _lastPower.description = text
        }

      case Block("P#flavorIndent", Emphasis(aspect) :: Text(desc) :: Nil) =>
      //TODO MM3 power aspect

      case Block("P#flavor", Emphasis(comment) :: Nil) if (_lastPower != null) =>
        // A secondary attack
        _powerSupplement = comment
      //_lastPower.secondaryKeywords = if(comment == "") null else comment

      case Block("P#flavor", parts@Key("Description") :: SingleTextBreakToNewLine(text)) =>
        monster.set("Description", text)

      case Block("P#", Emphasis(text) :: Nil) => monster.set("comment", (text))
      case NonBlock(dontcare) => // Don't care
      case Table("bodytable", cellsRaw) =>
        val cells = cellsRaw.map(cell => Cell(cell.clazz, cell.content.map(p => p.transform(Parser.TrimProcess))))
        val senses: Cell = cells(5) match {
          case b: Cell => b
          case s => throw new Exception("Should not have reached this point")
        }
        val taggedSenses = if(senses.content.isEmpty) senses else Cell(senses.clazz, Key("Senses") :: senses.content)
        val parts = cells.updated(5, taggedSenses).flatMap(e => e.content)
        processPrimaryBlock(partsToPairs(parts))
        monster.setMM3Format()

      case Block("H2#", dontcare) => //TODO Header for trait and actions
      case s =>
        throw new Exception("Failed to process: " + s)
        return false
    }
    true
  }
}