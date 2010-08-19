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

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import vcc.domain.dndi.Parser._
import vcc.infra.text.{TextSegment, TextBlock, StyledText}
import xml.Node

@RunWith(classOf[JUnitSuiteRunner])
class MonsterReaderTest extends JUnit4(MonsterReaderSpec)

object MonsterReaderSpec extends Specification {
  val reader = new MonsterReader(0)

  final val sampleDesc = StyledText(List(
    TextBlock("P","flavor",TextSegment("Something happens.")),
    TextBlock("P","flavorIndent",TextSegment.makeItalic("Secondary"))))

  // Returns a block stream with already advanced.
  def getBlockStream(xml: Node): TokenStream[BlockElement] = {
    val blk = Parser.parseBlockElement(xml, true)
    val blk2 = Block("P#flavor", List(Text("Something happens.")))
    val blk3 = Block("P#flavorIndent", List(Emphasis("Secondary")))
    val tailBlock = Block("POWEREND", Nil)

    val ts = new TokenStream(List(blk, blk2, blk3, tailBlock))
    ts.advance must beTrue
    ts
  }


  "MonsterReader.processTailBlock" should {
    "read description" in {
      val stream = new TokenStream[BlockElement](List(
        Block("P#flavor",List(Key("Description"),Text(": black and mean."), Break(), Text("Razor sharp")))
        ))
      stream.advance()
      val map = reader.processTailBlock(stream)

      map must haveKey("DESCRIPTION")
      map("DESCRIPTION") must_== "black and mean.\nRazor sharp"
    }
  }



  "MonsterReader.processPower" should {

    val mr = new MonsterReader(0)

    "read aura with keyword" in {
      val ts = getBlockStream(<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/aura.png" align="top"></IMG> <B>Spider Host</B> (Poison) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Aura</B> 1</P>)
      val power = mr.processPower(ActionType.Trait, ts)
      power must notBeNull

      power.definition must_== CompletePowerDefinition(Seq(IconType.Aura), "Spider Host", "(Poison)", AuraUsage(1))
      power.action must_== ActionType.Trait
      power.description must_== sampleDesc
    }

    "read power without keyword" in {
      val ts = getBlockStream(<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Darkfire</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B></P>)
      val power = mr.processPower(ActionType.Minor, ts)
      power must notBeNull

      power.definition must_== CompletePowerDefinition(Seq(IconType.Range), "Darkfire", null, EncounterUsage(1))
      power.action must_== ActionType.Minor
      power.description must_== sampleDesc
    }

  }

}