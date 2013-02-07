/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing

import org.uispec4j.interception.toolkit.UISpecDisplay
import concurrent.SyncVar
import org.specs2.specification.Scope
import org.specs2.mutable.BeforeAfter
import org.uispec4j.Panel

trait UISpec4JPanelScope extends Scope with BeforeAfter {
  private var adapterPanel: Panel = null

  def createPanelAdapter(): scala.swing.Panel

  def before {
    UISpecDisplay.instance.reset()
    adapterPanel = new Panel(createPanelAdapter().peer)
  }

  def after {
    adapterPanel = null
    UISpecDisplay.instance.rethrowIfNeeded()
    UISpecDisplay.instance.reset()
  }

  def getPanel: Panel = adapterPanel

  /**
   * Executes an dummy action on the AWT-EventQueue, which means any pending swing actions
   * will have been executed.
   */
  def syncWithSwing() {
    val ret = new SyncVar[Long]()
    SwingHelper.invokeLater{
      ret.put(10L)
    }
    ret.get
  }
}