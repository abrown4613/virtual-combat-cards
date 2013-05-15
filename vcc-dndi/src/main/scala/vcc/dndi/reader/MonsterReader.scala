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
package vcc.dndi.reader

import collection.immutable.List
import vcc.dndi.reader.Parser._
import vcc.dndi.reader.CompendiumCombatantEntityMapper._
import vcc.infra.text.{TextBlock, StyledText}
import util.matching.Regex
import org.exnebula.iteratee._

/**
 * Extract form the parts the usage of MM3 style usages
 */
object SomeUsage {
  val sep = Icon(IconType.Separator)

  private val encounterLimit = """(\d+)\s*/\s*Encounter""".r
  private val rechargeWithDice = """^\s*Recharge\s+(\d+).*""".r
  private val roundLimit = """^\s*(\d+)\s*/\s*round""".r
  private val rechargeAllBold = """^\s*Recharge\s+(.*)$""".r

  def unapply(parts: List[Part]): Option[Usage] = {
    parts match {
      case Nil => None
      case Key("") :: Nil => Some(NoUsage)
      case Key("Aura") :: Text(range) :: Nil => Some(AuraUsage(range.trim))
      case Key(`encounterLimit`(uses)) :: Nil => Some(EncounterUsage("%s/Encounter".format(uses)))
      case Key("Encounter") :: Text(detail) :: Nil => Some(EncounterUsage(detail.trim))
      case Key("Encounter") :: Nil => Some(EncounterUsage())
      case Text(`rechargeWithDice`(min)) :: Nil => Some(RechargeDiceUsage(min.toInt))
      case Key("Recharge") :: Text(condition) :: Nil => Some(RechargeConditionalUsage(condition.trim))
      case Key(`rechargeAllBold`(WithoutParenthesis(condition))) :: Nil => Some(RechargeConditionalUsage(condition.trim))
      case Key("At-Will") :: Nil => Some(AtWillUsage(None))
      case Key("At-Will") :: Text(WithoutParenthesis(`roundLimit`(n))) :: Nil => Some(AtWillUsage("%s/round".format(n)))
      case Key("At-Will") :: Text(WithoutParenthesis(detail)) :: Nil => Some(AtWillUsage(detail.trim))
      case _ => None
    }
  }
}

/**
 * Remove parenthesis if the string has.
 */
object WithoutParenthesis {
  private val parenthesis = """^\s*\((.*)\)\s*$""".r

  def unapply(text: String): Option[String] = {
    text match {
      case this.parenthesis(s) => Some(s.trim)
      case s => Some(s.trim)
    }
  }
}

object SomePowerDefinition {

  private object Keyword {
    def unapply(parts: List[Part]): Option[String] = parts match {
      case Nil => Some(null)
      case Key(text) :: Nil => Some(text)
      case _ => None
    }
  }

  private object NameKeyword {
    def unapply(parts: List[Part]): Option[(String, Option[String])] = parts match {
      case Key(name) :: Nil => Some(name.trim, None)
      case Key(name) :: Text(keyword) :: Nil => Some(name.trim, Some(keyword.trim))
      case _ => None
    }
  }

  def unapply(parts: List[Part]): Option[PowerDefinition] = {
    Option(parts match {
      case PowerHeaderParts(icons, NameKeyword(name, keyword), SomeUsage(usage)) =>
        CompletePowerDefinition(icons, name, keyword.getOrElse(null), usage)
      case PowerHeaderParts(icons, NameKeyword(name, actionUsage), Keyword(keywords)) =>
        LegacyPowerDefinition(icons, name, actionUsage.getOrElse(null), keywords, null)
      case _ => null
    })
  }
}

/**
 * Extracts the correct action from the H2 action type headers
 */
object SectionActionType {
  private val normalizedActionMap = ActionType.values.map(x => (x.toString.toLowerCase + "s", x)).toMap

  def unapply(parts: List[Part]): Option[ActionType.Value] = {
    parts match {
      case Text(text) :: Nil => normalizedActionMap.get(text.trim.toLowerCase)
      case _ => None
    }
  }
}

/**
 * Separates the power header into three parts, the Seq[IconType.Value] and parts before and after the Separator (if present).
 * Will also eliminates breaks in the header (some bad entries have that). Will replace the MM3 blank key at the end for
 * Separator blank key
 */
object PowerHeaderParts {
  private val whitespace = """^[\s\n\r\t]*$""".r

  private val separator = Icon(IconType.Separator)

  private val mm3BlankKey = Key("")

  def unapply(parts: List[Part]): Option[(Seq[IconType.Value], List[Part], List[Part])] = {
    val normalized = normalizeMM3EntryWithBlankKeyAtEnd(breakToSpace(parts).filter(isNotWhiteSpaceText))
    val (icons, nonIcons) = normalized.partition(p => p.isInstanceOf[Icon] && p != separator)
    val iconSeq = icons.map(p => p.asInstanceOf[Icon].iconType).toSeq
    val (names, usage) = splitPartListAt(nonIcons, nonIcons.indexOf(separator))
    Some((iconSeq, names, usage))
  }

  private def splitPartListAt(list: List[Part], position: Int) = {
    val sizeOfFirstList = if (position < 0) list.length else position
    (list.take(sizeOfFirstList), list.drop(sizeOfFirstList + 1))
  }

  private def normalizeMM3EntryWithBlankKeyAtEnd(cleanPart: List[Part]): List[Part] =
    if (cleanPart.last == mm3BlankKey) cleanPart.init ::: List(separator, mm3BlankKey)
    else cleanPart

  private def isNotWhiteSpaceText(part: Part) = part match {
    case Text(`whitespace`()) => false
    case _ => true
  }
}

/**
 * Reads DNDI monster entries in both MM3 and previous format
 */
class MonsterReader(id: Int) extends DNDIObjectReader[Monster] with BlockElementConsumer {
  final private val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final private val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")
  final private val perceptionMatcher = """^\s*[Pp]erception\s+([\+\-]?\d+).*$""".r
  final private val senseMatcher = """^.*;\s+(.+)$""".r
  private final val auraMatcher = """(\(.+\)\s+)?aura\s+(\d+)\s*;(.*)""".r

  private final val primaryStats = Set("Initiative", "Senses", "HP", "Bloodied",
    "AC", "Fortitude", "Reflex", "Will",
    "Immune", "Resist", "Vulnerable",
    "Saving Throws", "Speed", "Action Points", "Perception")

  def process(blocks: List[BlockElement]): Monster = {
    readMonster.consumeAll(blocks.filterNot(_.isInstanceOf[NonBlock])) match {
      case (Right(trap), Nil) => trap
      case (Right(trap), rest) => throw new UnexpectedBlockElementException("Unconsumed block: ", rest.head)
      case (Left(error), rest) => throw error
    }
  }

  private def readMonster = for {
    headMap <- readHeader
    monster <- (readMM3Entry(headMap) orElse readMM2Entry(headMap))
  } yield monster

  private def readHeader = matchConsumer("Expected H2 header") {
    case HeaderBlock("H1#monster", values) =>
      val headMap = normalizeTitle(values).toMap
      headMap.addDefaultFor("xp", "0").addDefaultFor("level", "1").map.
        updated("role", headMap.get("role").map(normalizeRole).getOrElse("No Role"))
  }

  private def readMM2Entry(headMap: Map[String, String]): Consumer[BlockElement, Monster] = for {
    statAndAuras <- readMM2PrimaryBlock
    legacyPowers <- readMM2Powers
    secondary <- readSecondaryStats
    tailStats <- repeat(matchTailBlocks)
  } yield {
    val (statMap, aurasAsTraits) = statAndAuras
    new Monster(id,
      normalizeCompendiumNames(headMap ++ statMap ++ secondary ++ flattenMaps(tailStats)),
      legacyPowers, Map(ActionType.Trait -> aurasAsTraits))
  }

  private def readMM3Entry(headMap: Map[String, String]): Consumer[BlockElement, Monster] = for {
    statMap <- readMM3PrimaryBlock
    powerByAction <- repeat(readActionGroupedPowers)
    secondary <- readSecondaryStats
    tailStats <- repeat(matchTailBlocks)
  } yield {
    new Monster(id,
      normalizeCompendiumNames(headMap ++ statMap ++ secondary ++ flattenMaps(tailStats)),
      Nil, flattenMaps(powerByAction))
  }

  private def readMM2PrimaryBlock = matchConsumer("MM2 header") {
    case Block("P#flavor", parts) =>
      val (stats, auras) = processPrimaryBlock(partsToPairs(trimParts(parts)))
      (normalizeLegacySenses(stats), auras)
  }

  private def readMM3PrimaryBlock = matchConsumer("MM3 primary table") {
    case Table("bodytable", cellsRaw) =>
      processPrimaryBlock(partsToPairs(cellsToKeyTextParts(cellsRaw)))._1
  }

  private def readSecondaryStats = matchConsumer("MM3 Secondary Stat Block") {
    case b@Block(tag, parts) if (matchSecondaryStatBlock(tag, parts)) => trimAndConvertToPairs(parts)
  }

  private def matchSecondaryStatBlock(tag: String, parts: List[Part]): Boolean =
    tag.startsWith("P#flavor alt") && parts.contains(Key("Str"))

  private def trimAndConvertToPairs(parts: List[Parser.Part]): Map[String, String] =
    partsToPairs(trimParts(parts)).map(p => p._1.toLowerCase -> p._2).toMap

  private def readActionGroupedPowers: Consumer[BlockElement, Map[ActionType.Value, List[Power]]] = for {
    action <- readActionType
    powers <- repeat(readPower(action))
  } yield Map(action -> powers)

  private def readActionType = matchConsumer("Match action type H2") {
    case Block("H2#", SectionActionType(actionType)) => actionType
  }

  private def readPower(action: ActionType.Value) = for {
    head <- readPowerHeader
    desc <- repeat(readPowerDescription)
  } yield Power(head, action, StyledText(desc))

  private def readPowerHeader = matchConsumer("Power header") {
    case b@Block("P#flavor alt", SomePowerDefinition(pdef)) => pdef
  }

  private def readPowerDescription = matchConsumer("P with power description") {
    case Block("P#flavor", parts) =>
      TextBlock("P", "flavor", ParserTextTranslator.partsToStyledText(parts): _*)
    case Block("P#flavorIndent", parts) =>
      TextBlock("P", "flavorIndent", ParserTextTranslator.partsToStyledText(parts): _*)
  }

  private def readMM2Powers = repeat(readPower(null))

  private def matchTailBlocks = matchConsumer[Map[String, String]]("TODO bunch of tail things") {
    case Block("P#flavor", Key("Description") :: SingleTextBreakToNewLine(text)) =>
      var ret = text.trim
      if (ret.startsWith(":")) ret = ret.tail
      Map("description" -> ret.trim)
    case b@Block(tag, parts) if (tag.startsWith("P#flavor")) =>
      trimAndConvertToPairs(parts)
    case Block(tag, commentPart :: Nil) if (tag.startsWith("P#")) =>
      simplifyComment(commentPart)
  }

  private def simplifyComment(commentPart: Part): Map[String, String] = {
    val comment: String = commentPart match {
      case Text(text) => text
      case Emphasis(text) => text
      case _ => null // Dont care much for this
    }
    optionalValueAsMap("comment", comment)
  }

  private def normalizeTitle(spanList: List[(String, String)]): List[(String, String)] = {
    spanList match {
      case ("xp", "-") :: rest => ("xp", "0") :: normalizeTitle(rest)
      case ("xp", this.reXP(xp)) :: rest => ("xp", xp) :: normalizeTitle(rest)
      case ("level", this.reLevel(lvl, role)) :: rest => ("level", lvl) ::("role", role) :: normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }

  /**
   * Extract primary stats form the a list of String pairs:
   */
  private def processPrimaryBlock(pairs: List[(String, String)]): (Map[String, String], List[Power]) = {
    val (attr, auras) = pairs.partition(pair => primaryStats(pair._1))
    (attr.map(p => (p._1.toLowerCase, p._2)).toMap, auras.map(p => promoteTraitLike(p._1, p._2)))
  }

  /**
   * Takes stats from primary block, that may be aura or regeneration and promotes them to the MM3
   * trait format.
   * @param name Key name (usually aura name or 'Regeneration')
   * @param desc Description of aura or generation
   * @return A new power.
   */
  private def promoteTraitLike(name: String, desc: String): Power = {
    desc match {
      case `auraMatcher`(keyword, range, auraDesc) =>
        Power(
          CompletePowerDefinition(Seq(IconType.Aura), name, if (keyword != null) keyword.trim else null, AuraUsage(range)),
          ActionType.Trait,
          StyledText.singleBlock("P", "flavorIndent", auraDesc.trim))
      case _ =>
        Power(
          CompletePowerDefinition(Seq(), name, null, NoUsage),
          ActionType.Trait,
          StyledText.singleBlock("P", "flavorIndent", desc))
    }
  }

  /**
   * Normalize perception and senses from a MM<3 senses expression. Normally in format: 'Perception +15; sense'. This
   * will split the senses and perception.
   * @param inMap The string from the file
   * @return Map with perception key and possibly a senses key
   */
  def normalizeLegacySenses(inMap: Map[String, String]): Map[String, String] = {
    if (inMap.isDefinedAt("senses")) {
      val senses = inMap("senses")
      val addMap = Map(
        "perception" -> Parser.TrimProcess(matchRegexOrDefault(senses, perceptionMatcher, "0"))) ++
        optionalValueAsMap("senses", matchRegexOrDefault(senses, senseMatcher, null))
      inMap - ("senses") ++ addMap
    } else
      inMap
  }

  private def optionalValueAsMap(key: String, value: String): Map[String, String] =
    if (value != null) Map(key -> value) else Map()

  private def matchRegexOrDefault(input: String, regex: Regex, default: String): String = input match {
    case `regex`(value) => value
    case _ => default
  }

  private def trimParts(parts: List[Part]): List[Part] = parts.map(p => p.transform(Parser.TrimProcess))

  private def cellsToKeyTextParts(cellsRaw: List[Cell]): List[Part] = {
    val cells = cellsRaw.map(cell => Cell(cell.clazz, cell.content.map(p => p.transform(Parser.TrimProcess))))
    val senses: Cell = cells(5)
    val taggedSenses = if (senses.content.isEmpty) senses else Cell(senses.clazz, Key("Senses") :: senses.content)
    cells.updated(5, taggedSenses).flatMap(e => e.content)
  }

  private def normalizeRole(role: String) = role match {
    case "Minion" => "No Role"
    case s if (s.startsWith("Minion ")) => s.substring(7)
    case other => other
  }

  implicit class MagicMap[K, V](val map: Map[K, V]) {
    def addDefaultFor(key: K, default: V): MagicMap[K, V] = if (map.isDefinedAt(key)) map else map.updated(key, default)
  }

  private def flattenMaps[K, V](maps: List[Map[K, V]]): Map[K, V] = maps.flatMap(identity).toMap
}