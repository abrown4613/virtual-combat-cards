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

import javax.swing.{AbstractAction, JButton, JFrame}
import java.awt.event.ActionEvent
import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import vcc.dnd4e.tracker.common.CombatState
import vcc.tracker.{Ruling, RulingContext}
import org.junit.Assert
import org.uispec4j._

class RulingDialogLayoutSample extends JFrame("Ruling Dialog Sample") {
  private var context: RulingContext[CombatState] = null
  private var dialogResult: Option[List[Ruling[CombatState, _, _]]] = null

  def setContext(context: RulingContext[CombatState]) {
    this.context = context
  }

  def getResult = dialogResult

  add(new JButton(new AbstractAction("Test") {
    def actionPerformed(e: ActionEvent) {
      val dialog = new RulingDialog(context, null)
      val result = dialog.promptUser()
      dialog.dispose()
      println("Result: " + result)
      dialogResult = dialog.dialogResult
    }
  }))
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

object RulingDialogLayoutSample {
  def main(args: Array[String]) {
    val frame = new RulingDialogLayoutSample()
    frame.setContext(RulingContext(
      CombatState.empty,
      null,
      Nil))
    frame.pack()
    frame.setVisible(true)
  }
}

trait WindowInterceptorWrapper {

  implicit def wrapInterceptor(interceptor: WindowInterceptor): RichWindow = new RichWindow(interceptor)

  trait WindowWrapperBuilder {
    def makeWindowHandler(): WindowHandler
  }

  case class clickButton(name: String) extends WindowWrapperBuilder {
    def makeWindowHandler(): WindowHandler = new WindowHandler() {
      def process(window: Window): Trigger = window.getButton(name).triggerClick()
    }
  }

  trait CommandEnd

  case object run extends CommandEnd

  class RichWindow(interceptor: WindowInterceptor) {

    def ~>(wrapper: WindowWrapperBuilder): RichWindow = {
      interceptor.process(wrapper.makeWindowHandler())
      this
    }

    def ~>(n: CommandEnd) {
      interceptor.run()
    }
  }

}

class RulingDialogTest extends UISpecTestCase with WindowInterceptorWrapper {

  var dialogOwner: RulingDialogLayoutSample = null

  override def setUp() {
    super.setUp()

    dialogOwner = new RulingDialogLayoutSample
    dialogOwner.pack()

    setAdapter(new UISpecAdapter {
      def getMainWindow: Window = {
        new Window(dialogOwner)
      }
    })
  }

  def testOpenWindowEmpty_thenCancel() {
    dialogOwner.setContext(RulingContext(CombatState.empty, null, Nil))
    showRulingDialog ~> clickButton("Cancel") ~> run
    Assert.assertEquals(None, dialogOwner.getResult)
  }

  def testOpenWindowEmpty_thenOK() {
    dialogOwner.setContext(RulingContext(CombatState.empty, null, Nil))
    showRulingDialog ~> clickButton("OK") ~> run
    Assert.assertEquals(Some(Nil), dialogOwner.getResult)
  }

  private def showRulingDialog = WindowInterceptor.init(getMainWindow.getButton("Test").triggerClick())
}