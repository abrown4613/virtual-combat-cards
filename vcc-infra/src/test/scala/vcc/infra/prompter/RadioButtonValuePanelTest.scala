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
package vcc.infra.prompter

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.uispec4j.finder.ComponentMatchers
import javax.swing.{JRadioButton, JLabel}
import org.uispec4j.UISpec4J
import vcc.util.swing.{UISpec4JPanelScope, SwingHelper}
import swing.Panel

class RadioButtonValuePanelTest extends SpecificationWithJUnit with Mockito {

  private val options = List("one", "two", "three")
  UISpec4J.init()

  trait context extends UISpec4JPanelScope{
    protected var panel: RadioButtonValuePanel = null

    def createPanelAdapter(): Panel = {
      panel = new RadioButtonValuePanel("text", options)
      panel
    }

    def getRadioButton = getPanel.getSwingComponents(ComponentMatchers.fromClass(classOf[JRadioButton])).toList.asInstanceOf[List[JRadioButton]]
  }

  "RadioButtonValuePanel" should {

    "Place label with collon" in new context {
      val l = getPanel.getSwingComponents(ComponentMatchers.fromClass(classOf[JLabel])).toList
      l.size must_== 1
      l(0).asInstanceOf[JLabel].getText must_== "text"
    }

    "add a check box for each item" in new context{
      val rbs = getRadioButton
      rbs.size must_== 3
      rbs.map(x => x.getText) must_== options
    }

    "update value on a click" in new context{
      val b = getPanel.getRadioButton("two")
      b must not beNull;
      b.click()
      Thread.sleep(200)
      panel.value must_== Some(1)
    }

    "fire of value change to listener" in new context{
      val mMediator = mock[ValuePanel.ChangeListener]
      panel.setListener(mMediator)
      getPanel.getRadioButton("two").click()
      there was one(mMediator).valuePanelChanged(RadioButtonValuePanel.Return(Some(1)))
    }

    "clear all option when value is set to null" in new context{
      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(2))
      }
      syncWithSwing()
      panel.value must_== Some(2)
      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(None)
      }
      syncWithSwing()
      getRadioButton.find(b => b.isSelected == true).isDefined must beFalse
      panel.value must_== None
    }
    "not fire value change when value is set externally" in new context{
      val mMediator = mock[ValuePanel.ChangeListener]
      panel.setListener(mMediator)

      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(0))
      }
      syncWithSwing()
      there was no(mMediator).valuePanelChanged(any)

      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(1))
      }
      syncWithSwing()
      panel.value must_== Some(1)
      there was no(mMediator).valuePanelChanged(any)
    }
  }
}