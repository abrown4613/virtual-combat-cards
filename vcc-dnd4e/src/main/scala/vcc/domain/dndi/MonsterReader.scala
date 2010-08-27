/*
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

import collection.immutable.List
import vcc.domain.dndi.Parser._
import vcc.infra.text.{TextBlock, TextBuilder, StyledText}
import util.matching.Regex
import collection.mutable.ListBuffer

object MonsterBlockStreamRewrite {
  val EndOfPower = Block("ENDPOWER", Nil)
}

class MonsterBlockStreamRewrite extends TokenStreamRewrite[BlockElement] {
  private var inPowerBlock = true

  private def isPowerEndBoundary(parts: List[Part]): Boolean = inPowerBlock && (parts contains Key("Str"))

  def rewriteToken(token: BlockElement): List[BlockElement] = {
    token match {
      case Block("P#flavor alt", parts) if (isPowerEndBoundary(parts)) =>
        inPowerBlock = false
        List(MonsterBlockStreamRewrite.EndOfPower, token)
      case NonBlock(_) => Nil // To capture some empty BR around the file
      case _ => List(token)
    }
  }
}

/**
 * Extract form the parts the usage of MM3 style usages
 */
object SomeUsage {
  val sep = Icon(IconType.Separator)

  object EncounterText {
    private val encounterRE = """(\d+)\s*/\s*Encounter""".r

    def unapply(text: String): Option[Int] = {
      text.trim match {
        case this.encounterRE(i) => Some(i.toInt)
        case "Encounter" => Some(1)
        case _ => None
      }
    }
  }

  private val rechargeWithDice = """^\s*Recharge\s+(\d+).*""".r
  private val roundLimit = """^\s*(\d+)\s*/\s*round""".r

  def unapply(parts: List[Part]): Option[Usage] = {
    parts match {
      case Nil => None
      case Key("") :: Nil => Some(NoUsage)
      case Key("Aura") :: Text(range) :: Nil => Some(AuraUsage(range.trim.toInt))
      case Key(EncounterText(n)) :: Nil => Some(EncounterUsage(n))
      case Text(`rechargeWithDice`(min)) :: Nil => Some(RechargeDiceUsage(min.toInt))
      case Key("Recharge") :: Text(condition) :: Nil => Some(RechargeConditionalUsage(condition.trim))
      case Key("At-Will") :: Nil => Some(AtWillUsage(0))
      case Key("At-Will") :: Text(`roundLimit`(n)) :: Nil => Some(AtWillUsage(n.toInt))
      case _ => None
    }
  }
}

object SomePowerDefinition {
  def unapply(parts: List[Part]): Option[PowerDefinition] = {
    val pdef: PowerDefinition = parts match {
      case PowerHeaderParts(icons, names, SomeUsage(usage)) =>
        names match {
          case Key(name) :: Nil => CompletePowerDefinition(icons, name.trim, null, usage)
          case Key(name) :: Text(keyword) :: Nil => CompletePowerDefinition(icons, name.trim, keyword.trim, usage)
          case _ => null // Must failover
        }
      case PowerHeaderParts(icons, names, afterDot) =>
        val keywords: Option[String] = afterDot match {
          case Nil => Some(null)
          case Key(text) :: Nil => Some(text)
          case _ => None
        }
        if (keywords.isDefined) {
          names match {
            case Key(name) :: Nil => LegacyPowerDefinition(icons, name.trim, null, keywords.get, null)
            case Key(name) :: Text(actionUsage) :: Nil =>
              LegacyPowerDefinition(icons, name.trim, actionUsage.trim, keywords.get, null)
            case _ => null
          }
        } else null // Something failed on the keywords parse
      case _ => null
    }
    if (pdef == null) {
      //TODO: Implement simplifed failover.
      throw new Exception("FAILOVER")
    } else {
      Some(pdef)
    }
  }
}

/**
 * Extracts the correct action from the H2 action type headers
 */
object SectionActionType {
  private val normalizedActionMap = ActionType.values.map(x => (x.toString.toLowerCase + "s", x)).toMap

  def unapply(parts: List[Part]): Option[ActionType.Value] = {
    parts match {
      case Text(text) :: Nil =>
        normalizedActionMap.get(text.trim.toLowerCase)
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

    val cleanPart = breakToSpace(parts).filter(part => part match {
      case Text(this.whitespace()) => false
      case _ => true
    })
    // Normalize MM3 entries, they always have a empty key at the end, add separator
    val normalized: List[Part] = if (cleanPart.last == mm3BlankKey) {
      cleanPart.take(cleanPart.length - 1) ::: List(separator, mm3BlankKey)
    } else cleanPart

    val (icons, rest) = normalized.partition(p => p.isInstanceOf[Icon] && p != separator)
    val iconSeq = icons.map(p => p.asInstanceOf[Icon].itype).toSeq
    val sep = rest.indexOf(separator)
    if (sep > 0) {
      val split = rest.splitAt(sep)
      Some((iconSeq.toSeq, split._1, split._2.tail))
    } else {
      Some((iconSeq.toSeq, rest, Nil))
    }
  }
}

/**
 * Reads DNDI monster entries in both MM3 and previous format
 */
class MonsterReader(id: Int) extends DNDIObjectReader[Monster]{
  final val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")

  private final val primaryStats = Set("Initiative", "Senses", "HP", "Bloodied",
    "AC", "Fortitude", "Reflex", "Will",
    "Immune", "Resist", "Vulnerable",
    "Saving Throws", "Speed", "Action Points", "Perception")


  private def normalizeTitle(l: List[(String, String)]): List[(String, String)] = {
    l match {
      case ("xp", this.reXP(xp)) :: rest => ("xp", xp) :: normalizeTitle(rest)
      case ("level", this.reLevel(lvl, role)) :: rest => ("level", lvl) :: ("role", role) :: normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }

  private[dndi] def processPower(action: ActionType.Value, stream: TokenStream[BlockElement]): Power = {
    val definition: PowerDefinition = stream.head match {
      case Block("P#flavor alt", SomePowerDefinition(pdef)) => pdef
      case _ => throw new UnexpectedBlockElementException("Expected P with class 'flavor alt'", stream.head)
    }
    stream.advance

    //From now on we expect P#flavor and P#flavorIndent
    val textBuilder = new TextBuilder()

    while (stream.head match {
      case Block("P#flavor", parts) =>
        textBuilder.append(TextBlock("P", "flavor", ParserTextTranslator.partsToStyledText(parts): _*))
        true
      case Block("P#flavorIndent", parts) =>
        textBuilder.append(TextBlock("P", "flavorIndent", ParserTextTranslator.partsToStyledText(parts): _*))
        true
      case _ =>
        false
    }) {
      stream.advance
    }
    Power(definition, action, textBuilder.getDocument)
  }

  /**
   * Process the header H1 entry and return a map of values.
   */
  private[dndi] def processHeader(stream: TokenStream[BlockElement]): Map[String, String] = {
    val headMap: Map[String, String] = stream.head match {
      case HeaderBlock("H1#monster", values) => normalizeTitle(values).toMap[String, String]
      case s => throw new UnexpectedBlockElementException("Expected H1 block", s)
    }
    stream.advance()
    var role = headMap("role")
    if(role == "Minion" ) role = "No Role"
    if(role.startsWith("Minion ")) role = role.substring(7)
    headMap + ("role" -> role)
  }

  /**
   * Extract primary stats form the a list of String pairs:
   */
  private def processPrimaryBlock(pairs: List[(String, String)]): (Map[String, String], List[(String, String)]) = {
    var auras:List[(String,String)] = Nil
    val attr = scala.collection.mutable.Map.empty[String,String]

    for ((k, v) <- pairs) {
      if (primaryStats(k)) attr.update(k.toLowerCase, v)
      else auras = (k, v) :: auras
    }
    (attr.toMap, auras.reverse)
  }

  private[dndi] def processMainStatBlock(stream: TokenStream[BlockElement]): (Boolean, Map[String, String], Seq[(String, String)]) = {
    stream.head match {
      case Table("bodytable", cellsRaw) =>
        val cells = cellsRaw.map(cell => Cell(cell.clazz, cell.content.map(p => p.transform(Parser.TrimProcess))))
        val senses: Cell = cells(5) match {
          case b: Cell => b
          case s => throw new Exception("Should not have reached this point")
        }
        val taggedSenses = if (senses.content.isEmpty) senses else Cell(senses.clazz, Key("Senses") :: senses.content)
        val parts = cells.updated(5, taggedSenses).flatMap(e => e.content)
        val (stats, whatever) = processPrimaryBlock(partsToPairs(parts))
        stream.advance()
        (true, stats, whatever)
      case Block("P#flavor", parts) =>
        val (stats, auras) = processPrimaryBlock(partsToPairs(trimParts(parts)))
        stream.advance()
        (false, normalizeLegacySenses(stats), auras)
      case _ =>
        throw new UnexpectedBlockElementException("Expected power Table, or P#flavor block; found:", stream.head)
    }
  }

  /**
   * Extract power in sections limited by H2
   */
  private[dndi] def processActionGroups(stream: TokenStream[BlockElement]): List[(ActionType.Value, List[Power])] = {
    var result = new ListBuffer[(ActionType.Value, List[Power])]
    while (stream.head match {
      case Block("H2#", SectionActionType(at)) =>
        stream.advance()
        val powers = processPowerGroup(at, stream)
        result += Pair(at, powers)
        true
      case MonsterBlockStreamRewrite.EndOfPower =>
        stream.advance()
        false
      case block =>
        throw new UnexpectedBlockElementException("Expected power H2 or ENDPOWER; found:", block)
    }) {
    }
    result.toList
  }

  private[dndi] def processLegacyPowers(stream: TokenStream[BlockElement]): List[Power] = {
    val powers = processPowerGroup(null, stream)
    stream.head match {
      case MonsterBlockStreamRewrite.EndOfPower =>
        stream.advance()
      case blk =>
        throw new UnexpectedBlockElementException("Expected ENDPOWER; found:", blk)
    }
    powers
  }

  /**
   * Extract powers until we hit ENDPOWER or an H2...
   */
  private[dndi] def processPowerGroup(action: ActionType.Value, stream: TokenStream[BlockElement]): List[Power] = {
    val result = new ListBuffer[Power]()
    while (stream.head match {
      case Block("P#flavor alt", _) =>
        val power = processPower(action, stream)
        result += power
        true
      case _ =>
        // This is not a new power, so let the caller figure out what to do.
        false
    }) {}
    result.toList
  }

  def process(blocks: List[BlockElement]): Monster = {
    val stream = new TokenStream[BlockElement](blocks, new MonsterBlockStreamRewrite())

    stream.advance()

    val headMap = processHeader(stream)
    val (isMM3Format, statMap, auras) = processMainStatBlock(stream)
    val powersByAction: List[(ActionType.Value, List[Power])] = if (isMM3Format) processActionGroups(stream) else Nil
    val legacyPowers: List[Power] = if (!isMM3Format) processLegacyPowers(stream) else Nil

    // Process tail.
    val tailStats = processTailBlock(stream)

    var aurasAsTraits = auras.map( x => promoteAuraLike(x._1,x._2)).toList
    var powersActionMap = powersByAction.toMap
    //Should need this, but just to be safe
    if (!aurasAsTraits.isEmpty) {
      powersActionMap = powersActionMap +
              (ActionType.Trait -> (powersActionMap.getOrElse(ActionType.Trait, Nil) ::: aurasAsTraits))
    }
    new Monster(id,
      CompendiumCombatantEntityMapper.normalizeCompendiumNames(headMap ++ statMap ++ tailStats),
      legacyPowers, powersActionMap.toMap)
  }

  private[dndi] def processTailBlock(stream: TokenStream[BlockElement]): Map[String, String] = {
    val result = scala.collection.mutable.Map.empty[String, String]
    while (stream.head match {
      case Block("P#flavor", parts@Key("Description") :: SingleTextBreakToNewLine(text)) =>
        var ret = text.trim
        if (ret.startsWith(":")) ret = ret.tail
        result.update("description", ret.trim)
        stream.advance()
      case Block("P#", commentPart :: Nil) =>
        val comment:String = commentPart match {
          case Text(text) => text
          case Emphasis(text) => text
          case _ => null // Dont care much for this
        }
        if(comment != null) result.update("comment", comment)
        stream.advance()
      case Block(tag, parts) if (tag.startsWith("P#flavor")) =>
        val normalizedParts = partsToPairs(trimParts(parts)).map(p => p._1.toLowerCase() -> p._2)
        result ++= normalizedParts
        stream.advance()
      case blk =>
        throw new UnexpectedBlockElementException("Expected P blocks in the trailing stat block, found:", blk)
    }) {

    }
    result.toMap
  }

  private final val auraMatcher = """(\(.+\)\s+)?aura\s+(\d+)\s*;(.*)""".r

  /**
   * Takes stats from primary block, that may be aura or regeneration and promotes them to the MM3
   * trait format.
   * @para name Key name (usually aura name or 'Regeneration')
   * @para desc Description of aura or generation
   * @return A new power.
   */
  private[dndi] def promoteAuraLike(name: String, desc: String): Power = {
    desc match {
      case `auraMatcher`(keyword, range, auraDesc) =>
        Power(
          CompletePowerDefinition(Seq(IconType.Aura), name, if (keyword != null) keyword.trim else null, AuraUsage(range.toInt)),
          ActionType.Trait,
          StyledText.singleBlock("P", "flavorIndent", auraDesc.trim))
      case _ =>
        Power(
          CompletePowerDefinition(Seq(), name, null, NoUsage),
          ActionType.Trait,
          StyledText.singleBlock("P", "flavorIndent", desc))
    }
  }

  final private val perceptionMatcher = """^\s*perception\s+([\+\-]?\d+).*$""".r
  final private val senseMatcher = """^.*;\s+(.+)$""".r

  /**
   * Normalize perception and senses from a MM<3 senses expression. Normally in format: 'Perception +15; sense'. This
   * will split the senses and perception.
   * @param inMap The string from the file
   * @return Map with perception key and possibly a senses key
   */
  def normalizeLegacySenses(inMap: Map[String, String]): Map[String, String] = {
    if (inMap.isDefinedAt("senses")) {
      val senses = inMap("senses").toLowerCase
      val per: String = senses match {
        case `perceptionMatcher`(value) => Parser.TrimProcess(value)
        case _ => "0"
      }
      val sense: String = senses match {
        case `senseMatcher`(value) => value
        case _ => null
      }
      val addMap = if (sense != null) Map("perception" -> per, "senses" -> sense) else Map("perception" -> per)
      inMap - ("senses") ++ addMap
    } else
      inMap
  }


  private def trimParts(parts: List[Part]): List[Part] = parts.map(p => p.transform(Parser.TrimProcess))

}