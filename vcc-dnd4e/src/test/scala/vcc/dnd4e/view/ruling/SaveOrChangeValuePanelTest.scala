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

import org.uispec4j.{Panel, UISpec4J, UISpecTestCase}
import org.mockito.Mockito._
import vcc.infra.prompter.{ValuePanel}

class SaveOrChangeValuePanelTest extends UISpecTestCase {

  import vcc.dnd4e.domain.tracker.common.SaveEffectSpecialDecision._

  UISpec4J.init()

  val thePanel = new SaveOrChangeValuePanel()
  val mListener = mock(classOf[ValuePanel.ChangeListener])
  thePanel.setListener(mListener)
  val uiPanel = new Panel(thePanel.peer)

  def testPanelBuiltCorrectly() {
    assertFalse(uiPanel.getTextBox("NewCondition").isEnabled)
    assertTrue(uiPanel.getRadioButton("Save").isVisible)
    assertTrue(uiPanel.getRadioButton("Change").isVisible)
    assertFalse(uiPanel.getButton("AcceptButton").isEnabled)
  }

  def testSetValueNone() {
    thePanel.setValue(None)
    testPanelBuiltCorrectly()
  }

  def testSetNewCondition() {
    thePanel.setField("NewCondition", "new condition")
    assertTrue(uiPanel.getTextBox("NewCondition").textEquals("new condition"))
  }

  def testSetValueSaved() {
    thePanel.setValue(Some(Saved))
    assertFalse(uiPanel.getTextBox("NewCondition").isEnabled)
    assertTrue(uiPanel.getRadioButton("Save").isSelected)
    assertFalse(uiPanel.getRadioButton("Change").isSelected)
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled)
  }

  def testSetValueChange() {
    thePanel.setValue(Some(Changed("new condition")))
    assertTrue(uiPanel.getTextBox("NewCondition").isEnabled)
    assertFalse(uiPanel.getRadioButton("Save").isSelected)
    assertTrue(uiPanel.getRadioButton("Change").isSelected)
    assertTrue(uiPanel.getTextBox("NewCondition").textEquals("new condition"))
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled)
  }

  def testCheckDisableNewConditionIfWeClickSaveAfterChange() {
    uiPanel.getRadioButton("Change").click()
    uiPanel.getRadioButton("Save").click()
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled)
    assertFalse(uiPanel.getTextBox("NewCondition").isEnabled)
  }

  def testClickOnSaveAndCallListener() {
    uiPanel.getRadioButton("Save").click()
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled)
    verify(mListener).valuePanelChanged(SaveOrChangeValuePanel.Value(Some(Saved)))
  }

  def testClickOnSaveSetValueCorrectly() {
    uiPanel.getRadioButton("Save").click()
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled)
    assert(thePanel.value == Some(Saved))
  }

  def testClickOnChangeEnableNewCondition() {
    uiPanel.getRadioButton("Change").click()
    assertTrue(uiPanel.getTextBox("NewCondition").isEnabled)
  }

  def testSettingChangeTextWithEnterCallsListener() {
    uiPanel.getRadioButton("Change").click()
    uiPanel.getTextBox("NewCondition").setText("worst", true)
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled) // Just for sync
    verify(mListener).valuePanelChanged(SaveOrChangeValuePanel.Value(Some(Changed("worst"))))
  }

  def testSettingChangeTextSetsValue() {
    uiPanel.getRadioButton("Change").click()
    uiPanel.getTextBox("NewCondition").setText("worst", false)
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled) // Just for sync
    assert(thePanel.value == Some(Changed("worst")))
  }

  def testSettingChangeTextWithoutEnterDoesNotCallsListener() {
    uiPanel.getRadioButton("Change").click()
    uiPanel.getTextBox("NewCondition").setText("worst", false)
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled) // Just for sync
    verify(mListener, never).valuePanelChanged(SaveOrChangeValuePanel.Value(Some(Changed("worst"))))
  }

  def testSettingChangeTextAddHitAcceptCallsListener() {
    uiPanel.getRadioButton("Change").click()
    uiPanel.getTextBox("NewCondition").setText("worst", false)
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled) // Just for sync
    uiPanel.getButton("AcceptButton").click()
    verify(mListener).valuePanelChanged(SaveOrChangeValuePanel.Value(Some(Changed("worst"))))
  }

  def testClickSaveThenAcceptCallsListenerTwice() {
    uiPanel.getRadioButton("Save").click()
    uiPanel.getButton("AcceptButton").click()
    assertTrue(uiPanel.getButton("AcceptButton").isEnabled) // Just for sync
    verify(mListener, times(2)).valuePanelChanged(SaveOrChangeValuePanel.Value(Some(Saved)))
  }
}