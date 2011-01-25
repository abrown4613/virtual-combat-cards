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

import javax.swing.BorderFactory
import vcc.util.swing.{CardPanel, MigPanel}
import swing.{Panel, ScrollPane, TextArea}
import vcc.infra.prompter.ValuePanel.Return
import java.awt.Color

/**
 * Controls how the MultiplePromptPanel is configured for a given prompt.
 */
trait PromptController {

  /**
   * Text that should be placed in the prompt window.
   */
  def prompt(): String

  /**
   * Informs what panel the controller wants to configure
   */
  def panelIdentity(): String

  /**
   * Setup panel that the controller selected.
   */
  def decoratePanel(panel: ValuePanel[_])

  /**
   * Action to be done when the user provides and input from the panel.
   */
  def handleAccept(value: ValuePanel.Return): Boolean

  /**
   * Indicate if an accept value has been provided and this prompt is considered answered.
   */
  def hasAnswer: Boolean
}

/**
 *   Compendium MultiplePromptPanel.
 */
object MultiplePromptPanel {

  /**
   * Listener that informs controlling dialog or panel that the input has been completed.
   */
  trait InputListener {
    /**
     * Prompt has been answered by user
     * @param controller The prompt controller that was answered
     */
    def answerProvided(controller: PromptController)
  }

}

/**
 * Composes a multiple panel prompt. Where the question is present in a text area, and one of the panel will be shown
 * depending on the what the PromptController inform.
 * @param listener Mandatory parameter of some object to be notified when the user inputs a valid reply for the prompt.
 */
class MultiplePromptPanel(listener: MultiplePromptPanel.InputListener) extends MigPanel("fill, ins 2") with ValuePanel.ChangeListener {

  private var panelMap: Map[String, ValuePanel[_]] = Map()

  private var controller: PromptController = null

  //Initialization
  border = BorderFactory.createEtchedBorder()
  private val promptText = new TextArea()
  private val cardPanel = new CardPanel()

  promptText.name = "promptText"
  promptText.border = BorderFactory.createEmptyBorder(2, 4, 2, 4)
  promptText.foreground = Color.BLACK
  promptText.focusable = false
  promptText.wordWrap = true
  promptText.lineWrap = true

  add(new ScrollPane(promptText), "h 50!, wrap, growx, growy")
  add(cardPanel, "growx, growy")

  /**
   * Use this method to register a Panel, should be done a construction time.
   * @param panel Must be a scala.swing.Panel that has the ValuePanel trait. Names must be unique.
   */
  protected[prompter] def addValuePanel(id: String, panel: Panel with ValuePanel[_]) {
    panelMap = panelMap.updated(id, panel)
    panel.setListener(this)
    cardPanel.addCard(panel, id)
  }

  def valuePanelChanged(newValue: Return) {
    if (controller.handleAccept(newValue))
      listener.answerProvided(controller)
  }

  /**
   * Update the panel to show a panel with the information controlled by the PromptController.
   * This will involve the following steps:
   * <ol>
   * <li>cntrl.prompt</li>
   * <li>cntrl.panelMap</li>
   * <li>cntrl.decoratePanel</li>
   * <li>panel.adjustFocus</li>
   * </ol>
   */
  def show(cntrl: PromptController) {
    this.controller = cntrl
    promptText.text = controller.prompt
    val panelId = controller.panelIdentity()
    if (panelMap.isDefinedAt(panelId)) {
      cardPanel.showCard(panelId)
      val panel = panelMap(panelId)
      controller.decoratePanel(panel.asInstanceOf[ValuePanel[AnyRef]])
      panel.adjustFocus()
    } else {
      throw new IllegalArgumentException("Panel name '" + panelId + "' is not defined.")
    }
  }
}