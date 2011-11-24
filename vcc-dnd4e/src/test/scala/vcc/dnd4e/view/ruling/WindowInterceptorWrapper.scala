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
package vcc.dnd4e.view.ruling

import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j.{Trigger, Window}
import org.uispec4j.assertion.{UISpecAssert, Assertion}

trait WindowInterceptorWrapper {

  implicit def wrapInterceptor(interceptor: WindowInterceptor): RichWindowInterceptor = new RichWindowInterceptor(interceptor)

  trait WindowWrapperBuilder {
    def makeWindowHandler(): WindowHandler
  }

  trait TriggerWrapper {
    def getTrigger(window: Window): Trigger
  }

  trait PartialTest {
    def executeTest(window: Window)
  }

  case class OngoingTest(interceptor: RichWindowInterceptor, tests: PartialTest*) {
    def ~>(next: TriggerWrapper): RichWindowInterceptor = {
      def wrap(): WindowWrapperBuilder = new WindowWrapperBuilder {
        def makeWindowHandler(): WindowHandler = new WindowHandler() {
          def process(window: Window): Trigger = {
            tests.foreach(_.executeTest(window))
            next.getTrigger(window)
          }
        }
      }
      interceptor ~> wrap()
      interceptor
    }

    def ~>(otherTest: PartialTest): OngoingTest = {
      OngoingTest(this.interceptor, tests ++ Seq(otherTest): _*)
    }
  }

  case class mustBeTrue(test: Window => Assertion) extends PartialTest {
    def executeTest(window: Window) {
      UISpecAssert.assertTrue(test(window))
    }
  }

  case class mustBeFalse(test: Window => Assertion) extends PartialTest {
    def executeTest(window: Window) {
      UISpecAssert.assertFalse(test(window))
    }
  }

  case class clickButton(name: String) extends WindowWrapperBuilder with TriggerWrapper {
    def makeWindowHandler(): WindowHandler = new WindowHandler() {
      def process(window: Window): Trigger = window.getButton(name).triggerClick()
    }

    def getTrigger(window: Window) = window.getButton(name).triggerClick()
  }

  trait CommandEnd

  case object run extends CommandEnd

  class RichWindowInterceptor(interceptor: WindowInterceptor) {

    def ~>(wrapper: WindowWrapperBuilder): RichWindowInterceptor = {
      interceptor.process(wrapper.makeWindowHandler())
      this
    }

    def ~>(n: CommandEnd) {
      interceptor.run()
    }

    def ~>(ongoing: PartialTest): OngoingTest = OngoingTest(this, ongoing)
  }


}