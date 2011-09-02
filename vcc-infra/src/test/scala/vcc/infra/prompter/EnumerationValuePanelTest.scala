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

import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationWithJUnit
import org.uispec4j.finder.{ComponentMatchers}
import javax.swing.{JRadioButton, JLabel}
import vcc.util.swing.{UISpec4JPanelScope, SwingHelper}
import swing.Panel
import org.uispec4j.UISpec4J

class EnumerationValuePanelTest extends SpecificationWithJUnit with Mockito {

  object SomeEnum extends Enumeration {
    val Yes = Value("Yes")
    val No = Value("No")
    val Maybe = Value("Maybe")
  }

  private val options = List("Yes", "No", "Maybe")

  UISpec4J.init()

  trait context extends UISpec4JPanelScope {
    protected var panel: EnumerationValuePanel[SomeEnum.type] = null

    def createPanelAdapter(): Panel = {
      panel = new EnumerationValuePanel[SomeEnum.type]("text", SomeEnum)
      panel
    }
    def getRadioButton = getPanel.getSwingComponents(ComponentMatchers.fromClass(classOf[JRadioButton])).toList.asInstanceOf[List[JRadioButton]]
  }

  "EnumerationValuePanel" should {

    "Place label with collon" in new context {
      val l = getPanel.getSwingComponents(ComponentMatchers.fromClass(classOf[JLabel])).toList
      l.size must_== 1
      l(0).asInstanceOf[JLabel].getText must_== "text"
    }

    "add a check box for each item" in new context {
      val rbs = getRadioButton
      rbs.size must_== 3
      rbs.map(x => x.getText) must_== options
    }

    "update value on a click" in new context {
      val b = getPanel.getRadioButton("Yes")
      b must not beNull;
      b.click()
      syncWithSwing()
      panel.value must_== Some(SomeEnum.Yes)
    }

    "fire of value change to listener" in new context {
      val mMediator = mock[ValuePanel.ChangeListener]
      panel.setListener(mMediator)
      getPanel.getRadioButton("No").click()
      there was one(mMediator).valuePanelChanged(EnumerationValuePanel.Value(Some(SomeEnum.No)))
    }

    "clear all option when value is set to null" in new context {
      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(SomeEnum.Maybe))
      }
      syncWithSwing()
      panel.value must_== Some(SomeEnum.Maybe)
      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(None)
      }
      syncWithSwing()
      getRadioButton.find(b => b.isSelected == true).isDefined must beFalse
      panel.value must_== None
    }

    "not fire value change when value is set externally" in new context {
      val mMediator = mock[ValuePanel.ChangeListener]
      panel.setListener(mMediator)

      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(SomeEnum.Yes))
      }
      syncWithSwing()
      there was no(mMediator).valuePanelChanged(any)

      SwingHelper.invokeInEventDispatchThread{
        panel.setValue(Some(SomeEnum.No))
      }
      syncWithSwing()
      panel.value must_== Some(SomeEnum.No)
      there was no(mMediator).valuePanelChanged(any)
    }
  }
}