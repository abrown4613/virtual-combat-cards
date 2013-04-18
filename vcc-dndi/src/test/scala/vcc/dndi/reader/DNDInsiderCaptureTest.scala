/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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

import org.specs2.SpecificationWithJUnit
import vcc.dndi.reader.DNDInsiderCapture.{UnsupportedEntity, CapturedEntity, NullEntityStore}
import java.io.ByteArrayInputStream
import org.specs2.execute.Result

class DNDInsiderCaptureTest extends SpecificationWithJUnit {
//  org.apache.log4j.BasicConfigurator.configure()

  def is = "capture logic".title ^
    "capture monster" ! capture(someMonster, "monster") ^
    "capture trap in new format" ! capture(newTrap, "trap") ^
    "handle missing id" ! missing ^
    "handled unsupported type" ! unsupported ^
    end

  private def missing = {
    captureContent(missingId) must_== None
  }

  private def unsupported = {
    captureContent(otherUnsupported) must_== Some(UnsupportedEntity(-1, "someOther"))
  }

  private def capture(content: String, itsClass: String): Result = {
    val result = captureContent(content)
    result match {
      case Some(CapturedEntity(ent)) =>
//        ent.dump(System.out)
        (ent.id must_== 123) and (ent.clazz must_== itsClass)
      case s =>
        failure
    }
  }

  private def captureContent(content: String): Option[DNDInsiderCapture.Result] = {
    DNDInsiderCapture.captureEntry(new ByteArrayInputStream(content.getBytes), NullEntityStore)
  }

  private val newTrap =
    """
      |<div id="123">
      |
      |		<h1 class="thHead">A trap<br/><span class="thSubHead">Object</span><br/><span class="thLevel">Level 5 Trap<span class="thXP">450</span></span></h1>
      |   <p class="thStat"><b>Detect</b> Perception or Arcana DC (hard)</p>
      |   <span class="thInit"><b>Initiative</b> +6</span>
      |   <p class="thStat"><b>Immune</b> attacks </p>
      |   <h2 class="thHead">Triggered Actions</h2>
      |   <p class="th2"><img src="http://www.wizards.com/dnd/images/symbol/Z3a.gif" /><b>Effect</b>  <img src="http://www.wizards.com/dnd/images/symbol/x.gif" /> <b>Daily</b></p>
      |   <p class="tbod"><i>Trigger</i>: snap.</p><p class="tbod"><i>Effect</i>: (Immediate Reaction): Ranged 1 (the triggering corpse); something.</p>
      |   <p class="publishedIn">Published in <a href="http://url.com/" target="_new">some source</a>.</p>
      |    </div>""".stripMargin

  private val someMonster =
    """
      |<div id="123">
      |
      |	 <h1 class="monster">Monster<br/><span class="type">Tiny natural beast</span><br/><span class="level">Level 1 Minion Skirmisher<span class="xp"> XP 0</span></span></h1>
      |  <table class="bodytable"><tr><td><b>HP</b> 1; a missed attack never damages a minion.</td><td class="rightalign"><b>Initiative</b> +6</td></tr><tr><td><b>AC</b> 13, <b>Fortitude</b> 9, <b>Reflex</b> 13, <b>Will</b> 10</td><td class="rightalign"><b>Perception</b>+1</td></tr><tr><td><b>Speed</b> 7</td><td class="rightalign">Low-light vision</td></tr><tr><td colspan="2"></td></tr></table>
      |  <h2>Traits</h2>
      |  <p class="flavor alt"> <b>power</b> <b></b></p><p class="flavorIndent">do something</p><p class="flavor alt"><b>Skills</b> Athletics +2, Stealth +9<br/> <b>Str</b> 4 (-3) <b>Dex</b> 19 (+4)  <b>Wis</b> 12 (+1)<br/><b>Con</b> 11 (0)  <b>Int</b> 2 (-4)  <b>Cha</b> 11 (0)</p>
      |  <p class="flavor"><b>Alignment</b> Unaligned<b> Languages</b> -</p>
      |  <p class="publishedIn">Published in <a href="http://www.wizards.com/" target="_new">Book</a>.</p>
      |</div>
    """.stripMargin

  private val otherUnsupported =
    """
      |<div id="1234">
      |
      |	 <h1 class="someOther">Monster<br/><span class="type">Tiny natural beast</span><br/><span class="level">Level 1 Minion Skirmisher<span class="xp"> XP 0</span></span></h1>
      |</div>
    """.stripMargin

  private val missingId =
    """
      |<div>
      |
      |	 <h1 class="monster">Monster<br/><span class="type">Tiny natural beast</span><br/><span class="level">Level 1 Minion Skirmisher<span class="xp"> XP 0</span></span></h1>
      |</div>""".stripMargin
}