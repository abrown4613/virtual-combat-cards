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
import vcc.infra.prompter.{MultiplePromptDialogController, MultiplePromptDialog}
import vcc.dnd4e.view.IconLibrary

/**
 * RulingDialog companion object
 */
object RulingDialog {
  def getInstanceAndController(frame: Frame): MultiplePromptDialogController = {
    new MultiplePromptDialogController(new RulingDialog(frame))
  }
}

/**
 * Dialog that provides panels to handle all ruling from the Tracker.
 */
class RulingDialog(frame: Frame) extends MultiplePromptDialog(frame, "Ruling that need your decision") {
  iconImage = IconLibrary.MetalD20.getImage
}