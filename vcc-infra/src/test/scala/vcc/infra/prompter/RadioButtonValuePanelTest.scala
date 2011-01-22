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

import org.specs.{Specification}
import org.uispec4j.{UISpecAdapter}
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.uispec4j.finder.{ComponentMatchers}
import javax.swing.{JRadioButton, JLabel}
import org.specs.mock.Mockito
import vcc.util.swing.{UISpec4JSpecification, SwingComponentWrapperAdapter, SwingHelper}

@RunWith(classOf[JUnitSuiteRunner])
class RadioButtonValuePanelTest extends JUnit4(RadioButtonValuePanelSpec)

object RadioButtonValuePanelSpec extends Specification with UISpec4JSpecification with Mockito {

  private var panel: RadioButtonValuePanel = null
  private val options = List("one", "two", "three")

  def getComponent(): UISpecAdapter = {
    panel = new RadioButtonValuePanel("text", options)
    new SwingComponentWrapperAdapter(panel)
  }

  def getRadioButton = getMainWindow.getSwingComponents(ComponentMatchers.fromClass(classOf[JRadioButton])).toList.asInstanceOf[List[JRadioButton]]

  "RadioButtonValuePanel" should {

    "Place label with collon" in {
      setAdapter(getComponent())
      val win = getMainWindow
      win mustNot beNull
      val l = win.getSwingComponents(ComponentMatchers.fromClass(classOf[JLabel])).toList
      l.size must_== 1
      l(0).asInstanceOf[JLabel].getText must_== "text:"
    }

    "add a check box for each item" in {
      setAdapter(getComponent())
      val win = getMainWindow
      val rbs = getRadioButton
      rbs.size must_== 3
      rbs.map(x => x.getText) must_== options
    }

    "update value on a click" in {
      setAdapter(getComponent)
      val b = getMainWindow.getRadioButton("two")
      b mustNot beNull
      b.click()
      Thread.sleep(200)
      panel.value must_== Some(1)
    }

    "fire of value change to mediator" in {
      val mMediator = mock[ValuePanelChangeListener[Int]]
      setAdapter(getComponent)
      panel.setMediator(mMediator)
      getMainWindow.getRadioButton("two").click
      there was one(mMediator).valuePanelChanged(Some(1))
    }

    "clear all option when value is set to null" in {
      setAdapter(getComponent)

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
    "not fire value change when value is set externally" in {
      val mMediator = mock[ValuePanelChangeListener[Int]]
      setAdapter(getComponent)
      panel.setMediator(mMediator)

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