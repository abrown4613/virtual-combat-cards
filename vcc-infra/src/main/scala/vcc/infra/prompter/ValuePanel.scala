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

import swing.Panel

/**
 * Notifies observer of changes in this ValuePane
 */
trait ValuePanelChangeListener[T] {
  /**
   * Panel value has changed and been accepted by the user.
   */
  def valuePanelChanged(newValue: Option[T])
}

/**
 * A panel that defines on value
 */
trait ValuePanel[T] {

  private var listener: ValuePanelChangeListener[T] = null

  /**
   * Set object that wishes to receive notification on change on this panel.
   * Usually this will be the a MultiplePromptPanel. The listener will be triggered when
   * user produces a valid and accepted value.
   * @param listener Listener, only one can be set at a given time.
   */
  def setListener(listener: ValuePanelChangeListener[T]) {
    this.listener = listener
  }

  /**
   * Notify listener of a change in the value.
   */
  protected def notifyListener() {
    if (listener != null) listener.valuePanelChanged(value())
  }

  /**
   * Set value of the panel.
   * @param value To be set, if None will clear the input panel.
   */
  def setValue(value: Option[T])

  /**
   * Returns current value defined in the panel
   * @return None if no value has been set, or Some(v) is the Panel has some valid value.
   */
  def value(): Option[T]
}
