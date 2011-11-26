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
package vcc.dnd4e.view.dialog

import javax.swing.{AbstractAction, JButton, JFrame}
import java.awt.event.ActionEvent
import org.uispec4j.interception.WindowInterceptor
import org.junit.Assert
import org.uispec4j._
import org.mockito.Mockito._
import org.exnebula.swing.PromptPanel
import scala.WindowInterceptorWrapper
import vcc.dnd4e.view.ruling.SampleStateData
import vcc.dnd4e.view.dialog.PromptDialog.StaticModel

class PromptDialogLayoutSample extends JFrame("Ruling Dialog Sample") {

  private var dialogModel: PromptDialog.Model = null

  def setModel(model: PromptDialog.Model) {
    this.dialogModel = model
  }

  private var dialogResult: Option[Boolean] = null

  def getResult = dialogResult

  add(new JButton(new AbstractAction("Test") {
    def actionPerformed(e: ActionEvent) {
      val dialog = new PromptDialog(dialogModel, null)
      val result = dialog.promptUser()
      dialog.dispose()
      println("Result: " + result)
      dialogResult = dialog.dialogResult
    }
  }))
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

object PromptDialogLayoutSample extends SampleStateData {

  def main(args: Array[String]) {
    val frame = new PromptDialogLayoutSample()
    frame.setModel(StaticModel(
      dialogTitle = "What is your choice",
      prompts = List(
        new RadioPromptPanel[Int]("Which size", RadioPromptPanel.Choice("Sustain Effect", 1), RadioPromptPanel.Choice("Cancel Effect", 0)),
        new RadioPromptPanel[Int]("How many", RadioPromptPanel.Choice("One", 1), RadioPromptPanel.Choice("Two", 2), RadioPromptPanel.Choice("Three", 3))
      )))
    frame.pack()
    frame.setVisible(true)
  }
}

class PromptDialogTest extends UISpecTestCase with WindowInterceptorWrapper with SampleStateData {

  var dialogOwner: PromptDialogLayoutSample = null
  var mockModel: PromptDialog.Model = null

  override def setUp() {
    super.setUp()

    mockModel = mock(classOf[PromptDialog.Model])
    doReturn(Nil).when(mockModel).prompts
    dialogOwner = new PromptDialogLayoutSample
    dialogOwner.setModel(mockModel)
    dialogOwner.pack()

    setAdapter(new UISpecAdapter {
      def getMainWindow: Window = {
        new Window(dialogOwner)
      }
    })
  }

  def testOpenWindowEmpty_thenCancel() {
    showRulingDialog ~> clickButton("Cancel") ~> go
    Assert.assertEquals(None, dialogOwner.getResult)
  }

  def testOpenWindowEmpty_thenOK() {
    showRulingDialog ~> clickButton("OK") ~> go
    Assert.assertEquals(Some(true), dialogOwner.getResult)
  }

  def testTitleWhenDialog_isInStartRoundContext() {
    doReturn("The dialog title").when(mockModel).dialogTitle
    showRulingDialog ~> mustBeTrue(_.titleEquals("The dialog title")) ~>
      clickButton("Cancel") ~> go
  }

  def testPromptForSustainEffectIsWellFormed() {
    val panel1 = new RadioPromptPanel[Int]("Which size", RadioPromptPanel.Choice("Sustain Effect", 1), RadioPromptPanel.Choice("Cancel Effect", 0))
    doReturn(List[PromptPanel](panel1)).when(mockModel).prompts
    showRulingDialog ~> mustBeTrue(_.getRadioButton("Sustain Effect").isEnabled) ~>
      mustBeTrue(_.getRadioButton("Cancel Effect").isEnabled) ~>
      mustBeFalse(_.getButton("Ok").isEnabled) ~>
      clickButton("Cancel") ~> go
  }

  private def showRulingDialog = WindowInterceptor.init(getMainWindow.getButton("Test").triggerClick())
}