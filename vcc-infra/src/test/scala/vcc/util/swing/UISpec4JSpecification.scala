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
package vcc.util.swing

import org.specs.Specification
import org.uispec4j.{UISpec4J, UISpecAdapter}
import org.uispec4j.interception.toolkit.UISpecDisplay
import concurrent.SyncVar

trait UISpec4JSpecification {
  spec: Specification =>
  private var adapter: UISpecAdapter = null

  UISpec4J.init()

  val swingContext = SpecContext() before {
    UISpecDisplay.instance.reset
  } after {
    adapter = null
    UISpecDisplay.instance.rethrowIfNeeded
    UISpecDisplay.instance.reset
  }
  swingContext(spec)

  def setAdapter(adapter: UISpecAdapter): Unit = {
    this.adapter = adapter
  }

  private def getAdapter(): UISpecAdapter = adapter

  def getMainWindow() = getAdapter.getMainWindow

  /**
   * Executes an dummy action on the AWT-EventQueue, which means any pending swing actions
   * will have been executed.
   */
  def syncWithSwing() {
    val ret = new SyncVar[Long]()
    SwingHelper.invokeLater{
      ret.set(10L)
    }
    ret.get
  }
}