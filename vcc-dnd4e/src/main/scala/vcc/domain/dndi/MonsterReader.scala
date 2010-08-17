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

class MonsterBlockStreamRewrite extends TokenStreamRewrite[BlockElement] {
  private var inPowerBlock = true

  private def isPowerEndBoundary(parts: List[Part]): Boolean = inPowerBlock && (parts contains Key("Str"))

  def rewriteToken(token: BlockElement): List[BlockElement] = {
    token match {
      case Block("P#flavor alt", parts) if (isPowerEndBoundary(parts)) =>
        inPowerBlock = false
        List(Block("ENDPOWER", Nil), token)
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

case class CompletePowerDefinition(icons: Seq[IconType.Value], name:String, keyword: String, usage: Usage) extends PowerDefinition

case class LegacyPowerDefinition(icons: Seq[IconType.Value], name:String, actionUsage: String, keyword: String, usage:Usage) extends PowerDefinition

case class Power(definition: PowerDefinition, action: ActionType.Value, description: StyledText)

/**
 * Extract form the parts the usage of MM3 style usages
 */
object SomeUsage {
  val sep = Icon(IconType.Separator)

  object EncounterText {
    private val encounterRE = """(\d+)\s*/\s*Encounter""".r
    def unapply(text:String):Option[Int] = {
      text.trim match {
        case this.encounterRE(i) => Some(i.toInt)
        case "Encounter" => Some(1)
        case _ => None
      }
    }
  }

  private val rechargeWithDice = """^\s*Recharge\s+(\d+).*""".r
  private val roundLimit = """^\s*(\d+)\s*/\s*round""".r
  def unapply(parts:List[Part]):Option[Usage] = {
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
    val pdef:PowerDefinition = parts match {
      case PowerHeaderParts(icons, names, SomeUsage(usage)) =>
        names match {
          case Key(name) :: Nil => CompletePowerDefinition(icons, name.trim, null, usage)
          case Key(name) :: Text(keyword) :: Nil => CompletePowerDefinition(icons, name.trim, keyword.trim, usage)
          case _ => null // Must failover
        }
      case PowerHeaderParts(icons, names, afterDot) =>
        val keywords:Option[String] = afterDot match {
          case Nil => Some(null)
          case Key(text) :: Nil => Some(text)
          case _ => None
        }
        if(keywords.isDefined) {
          names match {
            case Key(name) :: Nil => LegacyPowerDefinition(icons, name.trim, null, keywords.get, null)
            case Key(name) :: Text(actionUsage) :: Nil => LegacyPowerDefinition(icons, name.trim, actionUsage.trim, keywords.get, null)
            case _ => null
          }
        } else null // Something failed on the keywords parse
      case _ => null
    }
    if(pdef == null) {
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

class MonsterReader {
  private[dndi] def processPower(action: ActionType.Value, stream: TokenStream[BlockElement]): Power = {
    val definition: PowerDefinition = stream.head match {
      case Block("P#flavor alt", SomePowerDefinition(pdef)) => pdef
      case _ => throw new UnexpectedBlockElementException("Expected P with class 'flavor alt'",stream.head)
    }
    stream.advance

    //From now on we expect P#flavor and P#flavorIndent
    val textBuilder = new TextBuilder()

    while(stream.head match {
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
    Power(definition,action,textBuilder.getDocument)
  }
}