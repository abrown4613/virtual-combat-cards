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
package vcc.dnd4e.view.ruling

import org.mockito.Mockito._
import org.uispec4j.{Panel, UISpec4J, UISpecTestCase}
import vcc.infra.prompter.{TextFieldValuePanel, ValuePanel}

class DamageHealValuePanelTest extends UISpecTestCase {
  UISpec4J.init()

  private val listener = mock(classOf[ValuePanel.ChangeListener])
  private var panel = new DamageHealValuePanel("question")
  panel.setListener(listener)
  val uiPanel = new Panel(panel.peer)

  def testNotifyListenerOnChangeWithValidValue {
    val ef = uiPanel.getTextBox("editField")
    ef.setText("10", true)
    verify(listener, times(1)).valuePanelChanged(TextFieldValuePanel.Return(Some("10")))
  }

  def testNotifyListenerOnZeroHit {
    uiPanel.getButton("Zero").click
    verify(listener, times(1)).valuePanelChanged(TextFieldValuePanel.Return(Some("0")))
  }

  def testSetReturnValueOnZeroClick {
    uiPanel.getButton("Zero").click
    assert(panel.value == Some("0"))
  }

  def testValidatorAcceptGoodInput() {
    assert(UnsignedIntegerMatcher.asInt("   10 ") == Some(10))
  }

  def testValidatorRejectBadInput() {
    assert(UnsignedIntegerMatcher.asInt("   10a ") == None)
  }
}