/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.acceptance

import org.specs2.mutable.SpecificationWithJUnit
import java.io.File

class SaveAndRestoreCombatStateTest extends SpecificationWithJUnit {
  "Save and Restore Acceptance" should {
    "set comment" in {
      startApplication().
        setCombatComment("some comment").
        checkCombatantComment("some comment").
        close()
    }

    "set comment save, stop and restore" in {
      val saveFile = new File("test.save")

      startApplication().
        setCombatComment("some comment").
        checkCombatantComment("some comment").
        saveCombatState("test.save").
        close()

      saveFile.deleteOnExit()

      startApplication().
        loadCombatState("test.save").
        checkCombatantComment("some comment").
        close()
    }
  }

  private def startApplication(): ApplicationDriver = {
    new ApplicationDriver()
  }
}