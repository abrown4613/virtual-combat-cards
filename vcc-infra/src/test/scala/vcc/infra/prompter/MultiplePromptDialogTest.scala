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

import swing.{Action, MainFrame, Button}
import vcc.infra.prompter.ValuePanel.Return
import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j._
import assertion.Assertion


/**
 * MockQuestion
 */
class MockQuestion[T](val prompt: String, val panelIdentity: String, setup: PartialFunction[(ValuePanel[_], Option[T]), Unit]) extends PromptController {
  private var retVal: ValuePanel.Return = null
  private var lastValue: Option[T] = None

  def handleAccept(value: Return): Boolean = {
    value match {
    //This is a hack
      case RadioButtonValuePanel.Return(r) => lastValue = r.asInstanceOf[Option[T]]
      case TextFieldValuePanel.Return(r) => lastValue = r.asInstanceOf[Option[T]]
    }
    retVal = value
    true
  }

  def decoratePanel(panel: ValuePanel[_]) = if (setup.isDefinedAt(panel, lastValue)) {
    setup(panel, lastValue)
  } else {
    throw new IllegalArgumentException("Panel not of the correct type: " + prompt + "P: " + panel.getClass.getName)
  }

  def hasAnswer() = (retVal != null)

  def acceptedValue = lastValue

  /**
   * Little hack to chain tests
   */
  def clear() {
    lastValue = None
    retVal = null
  }
}

class MultiplePromptDialogTest extends UISpecTestCase {

  UISpec4J.init()

  private val yesNoPanelId = "YesNoPanel"
  private val numberPanelId = "NumberPanel"

  private var dialogReturn: Option[Boolean] = None

  class MainFrameAdapter(toPrompt: List[PromptController]) extends UISpecAdapter {
    def getMainWindow(): Window = {
      WindowInterceptor.run(new Trigger() {
        def run() {
          new MainFrame {
            val dialog = new MultiplePromptDialog(this, "Prompt") {
              addValuePanel(
                yesNoPanelId,
                new RadioButtonValuePanel("Accept this term", List("Yes", "No")))
              addValuePanel(
                numberPanelId,
                new TextFieldValuePanel("", x => try {
                  x.toInt
                  true
                } catch {
                  case _ => false
                }))

            }

            contents = new Button(Action("show") {
              val r = dialog.promptUser(toPrompt)
              dialogReturn = Some(r)
            })
          }.visible = true
        }
      })
    }
  }

  private val questions: List[MockQuestion[_]] = List(
    new MockQuestion[Int]("Question 1", yesNoPanelId, {
      case (r: RadioButtonValuePanel, lastValue) => r.setValue(lastValue)
    }),
    new MockQuestion[String]("Question 2", numberPanelId, {
      case (r: TextFieldValuePanel, lastValue) => r.setValue(lastValue)
    }),
    new MockQuestion[Int]("Question 3", yesNoPanelId, {
      case (r: RadioButtonValuePanel, lastValue) => r.setValue(lastValue)
    })
  )

  def testDialogProperlyOpened() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        assertTrue(window.titleEquals("Prompt"))
        assertTrue(window.getButton("Cancel").isEnabled)
        assertTrue(not(window.getButton("OK").isEnabled))
        assertTrue(window.getListBox.contains("Question 1", "Question 2", "Question 3"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testReturnFalseOnCancel() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        assertTrue(window.getButton("Cancel").isEnabled)
        window.getButton("Cancel").triggerClick()
    }
    assertTrue("Dialog Return is false", dialogReturn == Some(false))
  }

  def testFirstQuestionSelected() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        assertTrue(window.getListBox.selectionEquals("Question 1"))
        assertTrue(window.getTextBox("promptText").textEquals("Question 1"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testOnClickOfFirstAnswerStore() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        window.getButton("Cancel").triggerClick()
    }
    assertTrue("Question1 has set answer", questions(0).hasAnswer)
    assertTrue("Question1 has correct answer", questions(0).acceptedValue == Some(0))
  }

  def testOnClickOfFirstAnswerMoveToSecond() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        assertTrue(window.getListBox.selectionEquals("Question 2"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testSelectLastQuestionShouldSetupLast() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getListBox.selectIndex(2)
        assertTrue(window.getTextBox("promptText").textEquals("Question 3"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testSelectLastAndAnswer() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getListBox.selectIndex(2)
        window.getRadioButton("No").click()
        assertTrue("last question answered", questions(2).hasAnswer)
        assertTrue("last question correctly answered", questions(2).acceptedValue == Some(1))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testSelectThirdAnswerShouldGoToFirst() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getListBox.selectIndex(2)
        window.getRadioButton("Yes").click()
        assertTrue(window.getListBox.selectionEquals("Question 1"))
        assertTrue(window.getTextBox("promptText").textEquals("Question 1"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testAfterAllAnswersStayAtLastQuestion() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        window.getTextBox("editField").setText("10", true)
        window.getRadioButton("No").click()
        assertTrue(window.getListBox.selectionEquals("Question 3"))
        window.getButton("Cancel").triggerClick()
    }
  }

  def testAnswerAllOkShouldBeEnabled() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        window.getTextBox("editField").setText("10", true)
        window.getRadioButton("No").click()
        window.getButton("OK").triggerClick()
    }
  }

  def testClickingOkShouldReturnTrue() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        window.getTextBox("editField").setText("10", true)
        window.getRadioButton("No").click()
        window.getButton("OK").triggerClick()
    }
    assertTrue("Dialog OK returns true, not:" + dialogReturn, dialogReturn == Some(true))
  }

  def testAnswerCloseClearAndRedisplay() {
    setAdapter(new MainFrameAdapter(questions))
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        window.getRadioButton("Yes").click()
        window.getTextBox("editField").setText("10", true)
        window.getRadioButton("No").click()
        window.getButton("OK").triggerClick()
    }
    questions(1).clear() // Make the answer open
    doWithDialog(getMainWindow.getButton("show").triggerClick) {
      window =>
        assertTrue(window.getListBox.selectionEquals("Question 2")) // We cleared the second answer
        assertTrue(window.getTextBox("promptText").textEquals("Question 2"))
        assertTrue(not(window.getButton("OK").isEnabled))
        window.getButton("Cancel").triggerClick()
    }
  }

  private def doWithDialog(openTrigger: Trigger)(what: Window => Trigger) {
    val handler = new WindowHandler("first dialog") {
      def process(window: Window): Trigger = {
        what(window)
      }
    }
    WindowInterceptor.init(openTrigger).process(handler).run()
  }

  implicit def assertion(test: => Boolean): Assertion = {
    new Assertion {
      def check {
        if (!test) throw new AssertionError("failed test")
      }
    }
  }

}