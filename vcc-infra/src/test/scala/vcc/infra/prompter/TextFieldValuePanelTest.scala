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

import vcc.util.swing.{SwingComponentWrapperAdapter}
import org.uispec4j.{UISpec4J, UISpecTestCase}
import javax.swing.JLabel
import org.uispec4j.finder.ComponentMatchers
import org.uispec4j.assertion.Assertion
import org.mockito.Mockito
import org.mockito.Mockito._

class TextFieldValuePanelTest extends UISpecTestCase {

  private val validator = Mockito.mock(classOf[String => Boolean])
  when(validator.apply("")).thenReturn(false)
  when(validator.apply("abc")).thenReturn(false)
  when(validator.apply("10")).thenReturn(true)
  private val listener = Mockito.mock(classOf[ValuePanelChangeListener[String]])
  private var panel = new TextFieldValuePanel("question", validator)
  panel.setListener(listener)

  def makeAdapter() = {
    new SwingComponentWrapperAdapter(panel)
  }

  UISpec4J.init()

  def testLabelSetToQuestion() {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getSwingComponents(ComponentMatchers.fromClass(classOf[JLabel])).toList.head
    assertTrue(assertion("label must match", ef.asInstanceOf[JLabel].getText == "question"))
  }

  def testAcceptButtonNotEnable() {
    setAdapter(makeAdapter)
    val but = getMainWindow.getButton("acceptButton")
    assertTrue(not(but.isEnabled))
  }

  def testTextFieldPresentAndEditable() {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    assertTrue(ef.isEditable)
    assertTrue(ef.isEnabled)
  }

  def testInvokeValidatorOnChange() {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("abc")
    assertTrue(ef.textEquals("abc"))
    verify(validator, atLeastOnce()).apply("abc")
  }

  def testNotNotifyListenerOnChange {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("abc")
    verify(listener, never).valuePanelChanged(None)
  }

  def testNotNotifyListenerOnChangeWithValidValue {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10", false)
    verify(listener, never).valuePanelChanged(Some("10"))
  }

  def testKeepAcceptButtonDisabledOnBadInput {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("abc")
    val but = getMainWindow.getButton("acceptButton")
    assertTrue(not(but.isEnabled))
  }

  def testEnableAcceptButtonOnValidInput {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10")
    assertTrue(ef.textEquals("10"))
    val but = getMainWindow.getButton("acceptButton")
    assertTrue(but.isEnabled)
  }

  def testDisableAfterChangingToValidInput {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    val but = getMainWindow.getButton("acceptButton")
    ef.setText("10")
    assertTrue(ef.textEquals("10"))
    ef.setText("abc")
    assertTrue(not(but.isEnabled))
  }

  def testNotifyListenerWhenChangeIsValid {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10", true)
    verify(listener, atLeastOnce()).valuePanelChanged(Some("10"))
  }

  def testNotifyListenerWhenValueIsValidAndClickedAccept {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10", false)
    verify(listener, never()).valuePanelChanged(Some("10"))
    getMainWindow.getButton("acceptButton").click()
    verify(listener, atLeastOnce()).valuePanelChanged(Some("10"))
  }

  def testChangeBackgroundToShowProblem {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("abc")
    assertTrue(ef.backgroundEquals("FFE4C4"))
  }

  def testOnOpenWithBlankMustBeInvalid {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    assertTrue(ef.textEquals(""))
    assertTrue(ef.backgroundEquals("FFE4C4"))
    verify(validator, atLeastOnce()).apply("")
  }

  def testChangeBackgroundToShowOk {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10")
    assertTrue(ef.backgroundEquals("FFFFFF"))
  }

  def testReturnNoneValueOnInvalidInput {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("abc")
    assertTrue(assertion("Reply value as none on error", panel.value == None))
  }

  def testReturnSomeValueOnInvalidInput {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("10")
    assertTrue(assertion("Reply value as Some", panel.value == Some("10")))
  }

  def testWhenSettingValueValidateAndNotify {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    panel.setValue(Some("abc"))
    assertTrue(ef.textEquals("abc"))
    verify(validator, atLeastOnce()).apply("abc")
    verify(listener, never()).valuePanelChanged(None)
    assertTrue(assertion("Reply value as none on error", panel.value == None))
  }

  def testClearOnSetToNone {
    setAdapter(makeAdapter)
    val ef = getMainWindow.getTextBox("editField")
    ef.setText("1")
    assertTrue(ef.textEquals("1"))
    panel.setValue(None)
    assertTrue(ef.textEquals(""))
    verify(validator, atLeastOnce()).apply("")
    verify(listener, never()).valuePanelChanged(None)
    assertTrue(assertion("Reply value as none on error", panel.value == None))
  }

  def assertion(msg: String, test: => Boolean): Assertion = {
    new Assertion {
      def check {
        if (!test) throw new AssertionError(msg)
      }
    }
  }
}