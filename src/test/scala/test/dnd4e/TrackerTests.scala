//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package test.dnd4e

import junit.framework._

object TrackerTests {

  def suite: Test = {
    val suite = new TestSuite
    suite.addTestSuite(classOf[InitiativeSequenceTest])
    suite.addTestSuite(classOf[InitiativeRollerTest])
    suite.addTestSuite(classOf[EffectHandlerTest])
    suite.addTestSuite(classOf[ContextLoaderTest])
    suite
  }

  def main(args : Array[String]) {
    junit.textui.TestRunner.run(suite);
  }
}
