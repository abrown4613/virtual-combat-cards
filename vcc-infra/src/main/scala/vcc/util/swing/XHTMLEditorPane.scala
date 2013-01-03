/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import scala.swing._
import event.ButtonClicked

/**
 * Provides a split window with an Editor and a XHTMLPane side by side
 * @param startText The initial text on the editor pane
 * @param otherActions Additional action that can be added to the Pane.
 */
class XHTMLEditorPane(startText: String, otherActions: Action*) extends MigPanel("fill", "[600]", "[][][300]") {
  private val xhtmlPane = new XHTMLPane()
  private val editPane = new TextArea(startText)
  private val editScroll = new ScrollPane(editPane)
  private val wrapButton = new CheckBox("Wrap lines")
  private val split = new SplitPane(Orientation.Vertical, editScroll, xhtmlPane)

  editPane.lineWrap = true
  editPane.wordWrap = true
  wrapButton.selected = true
  split.oneTouchExpandable = true
  split.dividerLocation = 300
  split.resizeWeight = 0.75

  listenTo(wrapButton)
  reactions += {
    case ButtonClicked(wrapButton) =>
      editPane.lineWrap = wrapButton.selected
      editPane.wordWrap = wrapButton.selected
  }

  // Construction
  xhtmlPane.setDocumentFromText(startText)
  add(new Button(Action("Preview") {xhtmlPane.setDocumentFromText(editPane.text)}),
    if (otherActions.isEmpty) "wrap" else "split " + otherActions.length + 1)
  otherActions.foreach {x => add(new Button(x), if (x == otherActions.last) "wrap" else "")}
  add(wrapButton, "wrap")
  add(split, "growx,growy")

  def text: String = editPane.text

  def text_=(txt: String) {editPane.text = txt}

  def sync() {
    xhtmlPane.setDocumentFromText(editPane.text)
  }

}
