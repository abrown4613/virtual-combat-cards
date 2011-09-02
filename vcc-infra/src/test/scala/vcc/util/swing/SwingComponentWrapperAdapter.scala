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
package vcc.util.swing

import org.uispec4j.{Trigger, UISpecAdapter}
import org.uispec4j.interception.WindowInterceptor
import swing.{Frame, Component}

/**
 * This is a UISpecAdapter that wraps a single swing.scala.Component.
 * @param component Component to be wrapped.
 */
class SwingComponentWrapperAdapter(component: Component) extends UISpecAdapter {

  /**
   * Simple wrapper for a component to be used by the SwingComponentWrapperAdapter
   */
  class SwingComponentWrapper(val component: Component) extends Frame {
    contents = component
    visible = true
  }

  def getMainWindow: org.uispec4j.Window = {
    WindowInterceptor.run(new Trigger() {
      def run() {
        new SwingComponentWrapper(component)
      }
    })
  }
}


