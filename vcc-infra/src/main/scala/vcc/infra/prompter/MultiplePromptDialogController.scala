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

import concurrent.SyncVar
import vcc.util.swing.SwingHelper

/**
 * This trait is used to hide threading and other synchronization issues from calling function.
 */
trait MultiplePromptDialogController {
  def promptUser(toPrompt: List[PromptController]): Boolean
}

/**
 * Provide a method to call a MultiplePromptDialog in the swing Event thread and return it synchronously.
 * @param dialog The dialog to be called to provide prompt.
 */
class MultiplePromptDialogAsyncController(dialog: MultiplePromptDialog) extends MultiplePromptDialogController {
  def promptUser(toPrompt: List[PromptController]): Boolean = {
    val dialogReturn = new SyncVar[Boolean]
    SwingHelper.invokeInEventDispatchThread{
      dialogReturn.set(dialog.promptUser(toPrompt))
    }
    dialogReturn.get
  }
}