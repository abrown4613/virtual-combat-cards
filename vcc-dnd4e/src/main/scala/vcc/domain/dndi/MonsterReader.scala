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
      case _ => List(token)
    }
  }
}

object ActionType extends Enumeration {
  val Standard = Value("Standard Action")
  val Move = Value("Move Action")
  val Minor = Value("Minor Action")
  val Free = Value("Free Action")
  val No = Value("No Action")
  val Trait = Value("Trait")
}

trait Usage

case class AuraUsage(radius: Int) extends Usage

/**
 * At-Will usage, may be limited or unlimited
 * @para roundLimit If greater then 0, power can be used roundLimit times in a round, otherwise it is unlimited.
 */
case class AtWillUsage(roundLimit: Int) extends Usage

case class EncounterUsage(repeat: Int) extends Usage
object DailyUsage extends Usage
object NoUsage extends Usage
case class RechargeConditionalUsage(val condition: String) extends Usage
case class RechargeDiceUsage(val from: Int) extends Usage

abstract class PowerDefinition {
  val name: String
  val usage: Usage
}

case class CompletePowerDefinition(icons: Seq[IconType.Value], name: String, keyword: String, usage: Usage) extends PowerDefinition

case class LegacyPowerDefinition(icons: Seq[IconType.Value], name: String, actionUsage: String, keyword: String, usage: Usage) extends PowerDefinition

case class Power(definition: PowerDefinition, action: ActionType.Value, description: StyledText)

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
            case Key(name) :: Text(actionUsage) :: Nil => LegacyPowerDefinition(icons, name.trim, actionUsage.trim, keywords.get, null)
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
class MonsterReader(id: Int) {
  final val reXP = new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel = new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")

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
  private def processHeader(stream: TokenStream[BlockElement]): Map[String, String] = {
    val headMap: Map[String, String] = stream.head match {
      case HeaderBlock("H1#monster", values) => normalizeTitle(values).toMap[String, String]
      case s => throw new UnexpectedBlockElementException("Expected H1 block", s)
    }
    stream.advance()
    headMap
  }

  /**
   * Extract primary stats form the a list of String pairs:
   */
  private def processPrimaryBlock(pairs: List[(String, String)]): (Map[String, String], List[(String, String)]) = {
    //TODO Separate basic primary stats an the rest (which should be auras).
    /*
        for ((k, v) <- pairs) {
          if (primaryStats(k)) monster.set(k, v)
          else monster.addAura(k, v)
        }
    */
    (Map.empty[String, String], Nil)
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
        (true, stats, Seq())
      case Block("P#flavor", parts) =>
        val (stats, auras) = processPrimaryBlock(partsToPairs(trimParts(parts)))
        (false, stats, auras)
      case _ =>
        throw new UnexpectedBlockElementException("Expected power Table, or P#flavor block; found:", stream.head)
    }
  }


  private def getActionType(parts: List[Part]): ActionType.Value = {
    //TODO extract actionType form name
    null
  }

  /**
   * Extract power in sections limited by H2
   */
  private[dndi] def processActionGroups(stream: TokenStream[BlockElement]): List[(ActionType.Value, List[Power])] = {
    var result = new ListBuffer[(ActionType.Value, List[Power])]
    while (stream.head match {
      case Block("H2#", section) =>
        val at = getActionType(section)
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

    processHeader(stream)
    val (isMM3Format, statMap, auras) = processMainStatBlock(stream)
    val powersByAction: List[(ActionType.Value, List[Power])] = if (isMM3Format) processActionGroups(stream) else Nil
    val legacyPowers: List[Power] = if (!isMM3Format) processLegacyPowers(stream) else Nil

    // Process tail.
    val tailStats = processTailBlock(stream)
    //TODO Build monster o statMap, tailStats, powersByAction, legacyPower and Auras.

    null //TODO Add reply monster
  }

  private[dndi] def processTailBlock(stream: TokenStream[BlockElement]): Map[String, String] = {
    //TODO Complete the tail processing block for both MM3 and MM1 style
    /*
         case Block("P#flavor", parts@Key("Description") :: SingleTextBreakToNewLine(text)) =>
           monster.set("Description", text)

         case Block("P#", Emphasis(text) :: Nil) => monster.set("comment", (text))

         case Block("P#flavor alt", parts) =>
           parts match {
             case Key("Equipment") :: SingleTextBreakToSpace(text) =>
               monster.set("Equipment", text)

             case Key("Alignment") :: rest =>
               addToMap(partsToPairs(parts))

             //Blocks with Skills and Str or just Str
             case list if (list contains Key("Str")) =>
               addToMap(partsToPairs(parts))
          }
    */
    val result = scala.collection.mutable.Map.empty[String, String]
    while (stream.head match {
      case Block("P#flavor", parts@Key("Description") :: SingleTextBreakToNewLine(text)) =>
        var ret = text.trim
        if(ret.startsWith(":")) ret = ret.tail
        result.update("DESCRIPTION", ret.trim)
        stream.advance
    }) {
    }
    result.toMap
  }

  private def trimParts(parts: List[Part]): List[Part] = parts.map(p => p.transform(Parser.TrimProcess))

}