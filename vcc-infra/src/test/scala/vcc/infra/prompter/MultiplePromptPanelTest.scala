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

import org.uispec4j.{Panel, UISpec4J, UISpecTestCase}
import org.mockito.Mockito._
import org.junit.Assert.fail

class MultiplePromptPanelTest extends UISpecTestCase {

  UISpec4J.init()

  val targetPanel = "radioQuestion"

  val listener = mock(classOf[MultiplePromptPanel.InputListener])
  val thePanel = new RadioButtonValuePanel("Select Something?", List("Yes", "No"))
  val panel = new MultiplePromptPanel("Concerning:", listener) {
    addValuePanel("textQuestion", new TextFieldValuePanel("question", (x: String) => x.startsWith("a")))
    addValuePanel(targetPanel, thePanel)
  }
  val uiPanel = new Panel(panel.peer)

  val controller = mock(classOf[PromptController])
  when(controller.prompt).thenReturn("my prompt")
  when(controller.panelIdentity).thenReturn(targetPanel)

  def testPanelBuiltCorrectly() {
    assertTrue(uiPanel.getTextBox("promptText").isEnabled)
    assertTrue(uiPanel.getButton("acceptButton").isVisible)
    assertTrue(not(uiPanel.getButton("acceptButton").isEnabled))
    assertTrue(uiPanel.getTextBox("Concerning:").isVisible)
  }

  def testCallControllerSetPromptToReturnOfController() {
    panel.show(controller)
    assertTrue(uiPanel.getTextBox("promptText").textEquals("my prompt"))
    verify(controller).prompt
  }

  def testCallControllerGetPanel() {
    panel.show(controller)
    verify(controller).panelIdentity
  }

  def testControllerGetPanelAndThrowExceptionWhenPanelDoesNotExist() {
    when(controller.panelIdentity).thenReturn("not there panel")
    try {
      panel.show(controller)
      fail("should throw exception")
    } catch {
      case i: IllegalArgumentException =>
      case e => fail("Expected IllegalArgumentException")
    }
  }

  def testShowPanelInformedByController() {
    panel.show(controller)
    assertTrue(uiPanel.getRadioButton("Yes").isVisible)
  }

  def testDecorateThePanel() {
    panel.show(controller)
    verify(controller).decoratePanel(thePanel.asInstanceOf[ValuePanel[AnyRef]])
  }

  def testUserSelectionIsSentController() {
    panel.show(controller)
    uiPanel.getRadioButton("Yes").click()
    verify(controller).handleAccept(RadioButtonValuePanel.Return(Some(0)))
  }

  def testSendValidAcceptToCompletionListenerIfValid {
    panel.show(controller)
    when(controller.handleAccept(RadioButtonValuePanel.Return(Some(0)))).thenReturn(true)
    uiPanel.getRadioButton("Yes").click()
    assertTrue(uiPanel.getRadioButton("Yes").isSelected)
    verify(listener).answerProvided(controller)
  }

  def testNotSendValidAcceptToCompletionListenerIfNotValid {
    panel.show(controller)
    uiPanel.getRadioButton("Yes").click()
    verify(listener, never).answerProvided(controller)
  }
}