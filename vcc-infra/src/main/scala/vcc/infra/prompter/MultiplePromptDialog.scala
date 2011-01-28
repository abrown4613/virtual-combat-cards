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

import scala.swing._
import java.lang.String
import vcc.util.swing.{ModalFrame, MigPanel}

/**
 * This is a dialog window that houses a set of ValuePanel in it. You can send a series of questions/prompts via
 * PromptController. This dialog and they will be shown to the user. Action on the ValuePanel will result in
 * sent to the PromptController that is related to that prompt. If all prompts have been supplied answer the OK button
 * will be activated.
 * @param frame Controlling object
 * @param title Prompt title
 */
class MultiplePromptDialog(frame: Frame, title: String) extends ModalFrame(frame, title) with MultiplePromptPanel.InputListener {
  // Indicate that OK has been clicked
  private var accepted = false
  private val questionList = new ListView[PromptController](Nil) {
    renderer = ListView.Renderer(_.prompt)
    selection.intervalMode = ListView.IntervalMode.Single
  }
  private val questionPanel = new MultiplePromptPanel(this)
  private val okButton = new Button(Action("OK") {
    dismissDialog(true)
  })
  okButton.enabled = false

  contents = new MigPanel("fill, ins dialog", "[500]", "[100, grow 25][grow 75]10[grow 0, shrink 0]") {
    add(new ScrollPane(questionList), "wrap, h 40:60, growy, growx")
    add(questionPanel, "growx, growy, wrap")
    add(okButton, "split 3")
    add(new Button(Action("Cancel") {
      dismissDialog(false)
    }), "")
  }

  listenTo(questionList.selection)

  reactions += {
    case event.ListSelectionChanged(`questionList`, range, temporary) =>
      val selection = questionList.selection.items
      if (!selection.isEmpty) questionPanel.show(selection.head)
  }


  /**
   * Close window and set return value.
   */
  private def dismissDialog(accepted: Boolean) {
    this.accepted = accepted
    visible = false
  }

  private def showWindow() {
    this.minimumSize = contents(0).asInstanceOf[MigPanel].minimumLayoutSize
    this.preferredSize = contents(0).asInstanceOf[MigPanel].preferredLayoutSize
    this.size = preferredSize
    visible = true
  }

  /**
   * Show dialog and wait for use inputs on each prompt provided.
   * @param toPrompt List of PromptController one for each prompt to be asked from the user
   */
  def promptUser(toPrompt: List[PromptController]): Boolean = {
    questionList.listData = toPrompt
    nextUnanswered
    showWindow()
    this.accepted
  }

  /**
   * Add a ValuePanel as one of the options for the PromptControllers.
   * @param id String identifier of the panel
   * @param panel The panel to be added
   */
  protected def addValuePanel(id: String, panel: Panel with ValuePanel[_]) {
    questionPanel.addValuePanel(id, panel)
  }

  private def nextUnanswered() {
    val idx = questionList.listData.indexWhere(pc => !pc.hasAnswer)
    if (idx != -1) {
      questionList.selectIndices(idx)
      okButton.enabled = false
    } else {
      okButton.enabled = true
    }
  }

  def answerProvided(controller: PromptController) {
    nextUnanswered()
  }

}

