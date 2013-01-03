/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
import org.specs2.specification.AroundExample
import org.specs2.time.TimeConversions
import org.specs2.execute.Result
import org.specs2.execute.EventuallyResults._

trait RetryExamples extends AroundExample with TimeConversions {

  def around[R <% Result](r: =>R) =
    eventually(retries = 3, sleep = 10.millis)(r)

}

class SaveAndRestoreCombatStateTest extends SpecificationWithJUnit with RetryExamples {
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