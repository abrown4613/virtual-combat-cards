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

import vcc.controller.{Ruling, Decision}

/**
 * This trait acts as a PromptController but include the logic to extract the Decision from a given Ruling.
 */
trait RulingPromptController[R <: Ruling] extends PromptController {
  /**
   * @return Provide the Decision based on what was provided from use user input in the dialog.
   */
  def extractDecision(): Decision[R]
}

/**
 * This is a service class that is used to convert Ruling into appropriate RulingPromptController, which are used to
 * drive prompt and extract the user Decision after.
 */
trait RulingTranslatorService {

  /**
   * Convert a Ruling to a RulingPromptController. Is normally called from a RulingBroker.
   */
  def promptForRuling[R <: Ruling](ruling: Ruling): RulingPromptController[R]
}