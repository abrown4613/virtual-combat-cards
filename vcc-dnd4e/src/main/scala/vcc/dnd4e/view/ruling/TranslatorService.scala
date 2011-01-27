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
package vcc.dnd4e.view.ruling

import vcc.controller.Ruling
import vcc.infra.prompter.{RulingPromptController, RulingTranslatorService}

/**
 * TranslatorService companion object
 */
object TranslatorService {
  def getInstance(): RulingTranslatorService = new TranslatorService()
}

/**
 * Implementation of RulingTranslatorService that handle DND4E tracker Ruling RulingPromptController wrapper.
 */
class TranslatorService extends RulingTranslatorService {
  def promptForRuling[R <: Ruling](ruling: Ruling): RulingPromptController[R] = {
    ruling match {
    //TODO
      case _ => null
    }
  }
}