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

import java.awt.{Window, Toolkit}
import scala.swing._
import javax.swing.JDialog

/**
 * Application Modal Dialog frame. This is a base class that will block owning window.
 * @param window AWT window that owns this window (allows use by splash window)
 * @param title String title of the window
 */
class ModalFrame(window: Window, title: String) extends UIElement with RootPanel with Publisher {

  /**
   * Create a dialog based on top of scala.swing.Frame.
   * @param owner Frame that own this window and will be blocked
   * @param title Title of window
   */
  def this(owner: Frame, title: String) {
    this (owner.peer.getOwner, title)
  }

  override lazy val peer = new JDialog(window, title, java.awt.Dialog.ModalityType.APPLICATION_MODAL)

  /**
   * Center dialog on parent Frame.
   */
  def placeOnScreenCenter() {
    val screen = Toolkit.getDefaultToolkit.getScreenSize
    val height = preferredSize.height + 35
    val width = preferredSize.width + 15
    peer.setBounds((screen.width - width) / 2, (screen.height - height) / 2, width, height)
  }

  /**
   * Set Icon for the dialog.
   * @param img Image to be used as icon.
   */
  def iconImage_=(img: Image) {
    peer.setIconImage(img)
  }

}

/**
 * Create a user prompt dialog, this assumes that the dialog will return some value. Each dialog will have a
 * return type. Users should display the windows using {@code promptUser} method.
 * @param window AWT window that owns
 * @param title Dialog title
 *
 */
abstract class ModalPromptDialog[T](window: Window, title: String) extends ModalFrame(window, title) {

  def this(owner: Frame, title: String) {
    this (owner.peer.getOwner, title)
  }

  private var _result: Option[T] = None

  /**
   * Default ok Action which will correct result (using {@code collectResult}) and set the dialog invisible.
   */
  protected val okAction = Action("OK") {
    setDialogResultAndClose(collectResult())
  }

  /**
   * Close dialog with a Result of None
   */
  protected val cancelAction = Action("Cancel") {
    setDialogResultAndClose(None)
  }

  /**
   * This method is used to collect dialog information prior to sending the result out.
   * Implementation of this class must provide a implementation of this method.
   * @return None when the dialog has been cancels, Some when a value has been returned.
   */
  protected def collectResult(): Option[T]

  /**
   * Set the dialog result value
   */
  protected def dialogResult_=(v: Option[T]) {
    _result = v
  }

  /**
   * Returns the last value show by the dialog
   * @return Result of the dialog.
   */
  def dialogResult: Option[T] = _result

  /**
   * Set the result for the window and close it. Use this method if
   * you need other results to be applied.
   * @param result An option of the value to be returned
   */
  def setDialogResultAndClose(result: Option[T]) {
    dialogResult = result
    peer.setVisible(false)
  }

  /**
   * PromptUser for a result. Will clear previous result then show the dialog.
   * @return Return the value from the dialog (see {@code dialogResult}).
   */
  def promptUser():Option[T] = {
    dialogResult = None
    peer.pack()
    peer.setVisible(true)
    dialogResult
  }

  def dispose() {
    peer.dispose()
  }
}