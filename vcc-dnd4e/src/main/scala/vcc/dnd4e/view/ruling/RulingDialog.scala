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

import swing.Frame
import vcc.dnd4e.view.IconLibrary
import vcc.infra.prompter.{RadioButtonValuePanel, MultiplePromptDialogAsyncController, MultiplePromptDialogController, MultiplePromptDialog}

/**
 * RulingDialog companion object
 */
object RulingDialog {
  def getInstanceAndController(frame: Frame): MultiplePromptDialogController = {
    new MultiplePromptDialogAsyncController(new RulingDialog(frame))
  }

  val SimpleSavePanelIdentity = "SimpleSavePanel"
}

/**
 * Dialog that provides panels to handle all ruling from the Tracker.
 */
class RulingDialog(frame: Frame) extends MultiplePromptDialog(frame, "Ruling that need your decision") {
  this.iconImage_=(IconLibrary.MetalD20.getImage)
  addValuePanel(RulingDialog.SimpleSavePanelIdentity, new RadioButtonValuePanel("Saved?", List("Yes", "No")))
}