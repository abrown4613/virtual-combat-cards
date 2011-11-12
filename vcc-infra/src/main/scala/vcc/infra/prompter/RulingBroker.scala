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

import vcc.controller.{Decision, Ruling}

/**
 * This is a simple utility class that is responsible for translating Ruling to PromptController, sending them to a
 * dialog and then extracting the Decision from the reply (if OK was pressed).
 * @param dialogController A controller to show and wait for the answer fo the dialog
 * @param translator A service that converts Ruling to RulingPromptController
 */
class RulingBroker(dialogController: MultiplePromptDialogController, translator: RulingTranslatorService) {

  /**
   * Get Ruling convert to prompts and ask Dialog for user input
   * @param rulings Ruling that should be asked to the user
   */
  def promptRuling(promptContext: String, rulings: List[Ruling]): List[Decision[_ <: Ruling]] = {
    val prompts = rulings.map(r => translator.promptForRuling(r))
    if (dialogController.promptUser(promptContext, prompts)) {
      prompts.map(p => p.extractDecision)
    } else {
      Nil
    }
  }
}