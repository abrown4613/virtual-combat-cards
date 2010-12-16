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
package vcc.controller

import org.uispec4j._
import interception.{WindowHandler, WindowInterceptor}
import vcc.util.swing.SwingHelper
import concurrent.SyncVar

class PrompterTest extends UISpecTestCase {
  UISpec4J.init()

  class StubMainFrameAdapter(action: CommandSource => Unit) extends UISpecAdapter {
    def getMainWindow(): Window = {
      WindowInterceptor.run(new Trigger() {
        def run() {
          new StubMainFrame(action)
        }
      })
    }
  }


  def getResult(window: Window) = {
    val label = window.getTextBox("ResultLabel")
    assert(label != null)
    label
  }

  def testOpenBaseWindowAndCheckReady() {
    setAdapter(new StubMainFrameAdapter(null))
    val panel = getMainWindow()
    val but = panel.getButton("ActionButton")
    assert(but != null)
    assert(but.getLabel == "Action")

    val label = getResult(panel)
    assert(label.getText == "Waiting")
  }

  def testExecuteActionButton() {
    setAdapter(new StubMainFrameAdapter(source => {
      println("Action in thread: " + Thread.currentThread())
      source.provideDecisionsForRulings(Nil)
    }))

    val panel = getMainWindow()
    val but = panel.getButton("ActionButton")

    val handler = new WindowHandler("first dialog") {
      def process(window: Window): Trigger = {
        println("Window is: " + window.getName)
        return window.getButton("Yes").triggerClick();
      }
    }

    WindowInterceptor.init(but.triggerClick()).process(handler).run()

    val label = getResult(panel)
    assert(label.getText == "Complete")
  }

  /**
   * This is a simple test to check dispatching thing on the SwingEvent thread and read the result.
   * TODO Delete this
   */
  def testDispatchOnAnotherThread() {
    setAdapter(new StubMainFrameAdapter(null))
    val ret = new SyncVar[Long]()
    val t0 = System.currentTimeMillis();
    SwingHelper.invokeInEventDispatchThread {
      println("Start EventDispatch: " + (System.currentTimeMillis - t0) + "ms, I'm running here: " + Thread.currentThread())
      Thread.sleep(100);
      val id = Thread.currentThread().getId
      ret.set(10L)
      println("End of EventDispatch: " + (System.currentTimeMillis - t0) + "ms, ID = " + id)
    }
    println("After dispatch: " + (System.currentTimeMillis - t0) + "ms, Thread; " + Thread.currentThread())
    assert(ret.get == 10)
    println("Get was done: " + (System.currentTimeMillis - t0) + "ms, Thread; " + Thread.currentThread())
  }
}