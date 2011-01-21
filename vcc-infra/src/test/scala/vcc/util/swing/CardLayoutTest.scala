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
//$Id
package vcc.util.swing

import org.uispec4j.{UISpec4J, UISpecTestCase}
import swing.{Button}
import org.uispec4j.finder.ComponentMatchers
import javax.swing.JButton
import org.uispec4j.assertion.Assertion

class CardLayoutTest extends UISpecTestCase {

  UISpec4J.init()

  val cardPanel = new CardPanel()
  List("one", "two", "three").foreach(label => cardPanel.addCard(new Button(label), label))

  def testShowFirstTab() {
    setAdapter(new SwingComponentWrapperAdapter(cardPanel))

    assertTrue(assertion("Button one is visible")(buttonLabels() == List("one")))
  }

  def testShowSecondTab() {
    setAdapter(new SwingComponentWrapperAdapter(cardPanel))
    cardPanel.showCard("two")
    assertTrue(assertion("Button two is visible")(buttonLabels() == List("two")))
  }

  def testShowThreeTab() {
    setAdapter(new SwingComponentWrapperAdapter(cardPanel))
    cardPanel.showCard("three")
    assertTrue(assertion("Button three is visible")(buttonLabels() == List("three")))
  }


  def testNoChangeOnMissingCard() {
    setAdapter(new SwingComponentWrapperAdapter(cardPanel))
    cardPanel.showCard("not found")
    assertTrue(assertion("Button one is visible")(buttonLabels() == List("one")))
  }

  def buttonLabels(): List[String] = {
    getMainWindow().getSwingComponents(ComponentMatchers.fromClass(classOf[JButton])).toList.map(x => x.asInstanceOf[JButton].getText)
  }

  def assertion(msg: String)(test: => Boolean): Assertion = {
    new Assertion {
      def check {
        if (!test) throw new AssertionError(msg)
      }
    }
  }
}