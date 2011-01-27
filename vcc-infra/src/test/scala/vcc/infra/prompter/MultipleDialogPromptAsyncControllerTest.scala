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
//$Id$
package vcc.infra.prompter

import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j._
import swing.{Frame, FlowPanel}
import javax.swing.SwingUtilities
import concurrent.SyncVar
import junit.framework.Assert.{fail}

class MultipleDialogPromptAsyncControllerTest extends UISpecTestCase {

  UISpec4J.init()

  private val yesNoPanelId = "YesNoPanel"

  //val panel = new FlowPanel()
  val frame = new Frame()
  //val uiPanel = new Panel(panel.peer)

  val dialog = new MultiplePromptDialog(frame, "Prompt") {
    addValuePanel(
      yesNoPanelId,
      new RadioButtonValuePanel("Accept this term", List("Yes", "No")))
  }
  val question = List(
    new MockQuestion[Int]("Question 1", yesNoPanelId, {
      case (r: RadioButtonValuePanel, lastValue) => r.setValue(lastValue)
    }))

  val asyncController = new MultiplePromptDialogAsyncController(dialog)

  def testAsynchronousCallOfDialogAndCancel() {
    var ret: Option[Boolean] = None

    val barrier = onAnotherThereWaitOnBarrierAndDo(barrier => ret = Some(asyncController.promptUser(question)))

    doWithDialog(triggerBarrier(barrier)) {
      window =>
        window.getButton("Cancel").triggerClick()
    }
    assert(ret == Some(false))
  }

  def testAsynchronousCallOfDialogAndOk() {
    var ret: Option[Boolean] = None

    val barrier = onAnotherThereWaitOnBarrierAndDo(barrier => {
      ret = Some(asyncController.promptUser(question))
    })

    doWithDialog(triggerBarrier(barrier)) {
      window =>
        window.getRadioButton("Yes").click
        window.getButton("OK").triggerClick()
    }
    assert(ret == Some(true), "True should be result of closing window")
    assert(question.head.hasAnswer, "Answer must be set")
  }

  private def triggerBarrier(barrier: SyncVar[Boolean]): Trigger = {
    new Trigger() {
      def run {
        assert(SwingUtilities.isEventDispatchThread, "should be in swing")
        barrier.set(true)
      }
    }
  }

  private def onAnotherThereWaitOnBarrierAndDo(action: Boolean => Unit): SyncVar[Boolean] = {
    val barrier = new SyncVar[Boolean]
    val otherThread = new Thread() {
      override def run() {
        val b = barrier.get(1000)
        if (b.isDefined) action(b.get)
        else fail("Should have triggered barrier")
      }
    }
    otherThread.start()
    Thread.sleep(10)
    barrier
  }

  private def doWithDialog(openTrigger: Trigger)(what: Window => Trigger) {
    val handler = new WindowHandler("first dialog") {
      def process(window: Window): Trigger = {
        what(window)
      }
    }
    WindowInterceptor.init(openTrigger).process(handler).run()
  }

}