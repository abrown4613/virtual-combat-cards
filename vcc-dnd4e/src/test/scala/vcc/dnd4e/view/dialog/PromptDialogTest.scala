/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
import org.junit.Assert
import org.uispec4j._
import interception.{WindowHandler, WindowInterceptor}
import org.mockito.Mockito._
import org.exnebula.swing.PromptPanel
import vcc.dnd4e.view.dialog.PromptDialog.StaticModel
import java.lang.String

class PromptDialogLayoutSample extends JFrame("Ruling Dialog Sample") {
  private var dialogModel: PromptDialog.Model = null
  private var dialogResult: Boolean = false

  def setModel(model: PromptDialog.Model) {
    this.dialogModel = model
  }

  def getResult = dialogResult

  add(new JButton(new AbstractAction("Test") {
    def actionPerformed(e: ActionEvent) {
      dialogResult = PromptDialog.promptUserAndDismiss(dialogModel)
    }
  }))
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

object PromptDialogLayoutSample {

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

class PromptDialogTest extends UISpecTestCase {

  private val choice1 = RadioPromptPanel.Choice("Case 1", 1)
  private val choice2 = RadioPromptPanel.Choice("Case 2", 2)
  private val choice3 = RadioPromptPanel.Choice("Case 3", 3)

  private val panel1 = createPromptPanel("Which size", choice1, choice2)
  private val panel2 = createPromptPanel("Next prompt", choice2, choice3)

  private var dialogOwner: PromptDialogLayoutSample = null
  private var mockModel: PromptDialog.Model = null

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

  def testFixme() {
    println("WARNING: Disable Window Interception")
  }

  def pending_testOpenWindowEmpty_thenCancel() {
    showRulingDialog.process(clickButton("Cancel")).run()
    Assert.assertEquals(false, dialogOwner.getResult)
  }

  def pending_testOpenWindowEmpty_thenOK() {
    showRulingDialog.process(clickButton("Ok")).run()
    Assert.assertEquals(true, dialogOwner.getResult)
  }

  def pending_testTitleWhenDialog_fromModel() {
    doReturn("The dialog title").when(mockModel).dialogTitle
    showRulingDialog.process(new WindowHandler() {
      def process(window: Window): Trigger = {
        assertTrue(window.titleEquals("The dialog title"))
        window.getButton("Cancel").triggerClick()
      }
    }).run()
  }

  def pending_testSetupSimplePanel() {
    doReturn(List[PromptPanel](panel1)).when(mockModel).prompts
    showRulingDialog.process(new WindowHandler() {
      def process(window: Window): Trigger = {
        assertTrue(window.getRadioButton(choice1.name).isEnabled)
        assertTrue(window.getRadioButton(choice2.name).isEnabled)
        assertFalse(window.getButton("Ok").isEnabled)
        window.getButton("Cancel").triggerClick()
      }
    }).run()
  }

  def pending_testCompleteEditorEnablesOk() {
    doReturn(List[PromptPanel](panel1)).when(mockModel).prompts
    showRulingDialog.process(clickPromptRadioAntThenDismissWithOk(choice1.name)).run()
  }

  def pending_testCompleteEditor_andAnswerIsDefined() {
    doReturn(List[PromptPanel](panel1)).when(mockModel).prompts
    showRulingDialog.process(clickPromptRadioAntThenDismissWithOk(choice1.name)).run()
    Assert.assertEquals(Some(1), panel1.response)
  }
  def pending_testCompleteEditor_andAnswerIsDefined2() {
    val panel1 = createPromptPanel("Which size", choice1, choice2)
    doReturn(List[PromptPanel](panel1)).when(mockModel).prompts
    showRulingDialog.process(clickPromptRadioAntThenDismissWithOk(choice2.name)).run()
    Assert.assertEquals(Some(2), panel1.response)
  }

  def pending_testTwoPanel_whenCompleteEditorEnablesOk() {
    doReturn(List[PromptPanel](panel1, panel2)).when(mockModel).prompts
    showRulingDialog.process(new WindowHandler() {
      def process(window: Window): Trigger = {
        window.getRadioButton(choice1.name).click()
        assertFalse(window.getButton("Ok").isEnabled)
        window.getRadioButton(choice3.name).click()
        assertTrue(window.getButton("Ok").isEnabled)
        window.getButton("Ok").triggerClick()
      }
    }).run()
  }

  private def clickPromptRadioAntThenDismissWithOk(radioButtonName: String): WindowHandler = {
    new WindowHandler() {
      def process(window: Window): Trigger = {
        window.getRadioButton(radioButtonName).click()
        assertTrue(window.getButton("Ok").isEnabled)
        window.getButton("Ok").triggerClick()
      }
    }
  }

  private def createPromptPanel(promptTitle: String, choice: RadioPromptPanel.Choice[Int]*): RadioPromptPanel[Int] = {
    new RadioPromptPanel[Int](promptTitle, choice: _*)
  }

  private def clickButton(buttonName: String) = {
    new WindowHandler() {
      def process(window: Window): Trigger = {
        window.getButton(buttonName).triggerClick()
      }
    }
  }

  private def showRulingDialog = WindowInterceptor.init(getMainWindow.getButton("Test").triggerClick())
}