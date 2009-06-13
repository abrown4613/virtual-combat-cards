/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import vcc.dnd4e.BootStrap

class CombatantActionPanel(val uia:actors.Actor,val tracker:actors.Actor) extends BorderPanel with view.ContextualView[ViewCombatant] with view.SequenceView[ViewCombatant]{
  minimumSize=new java.awt.Dimension(
    BootStrap.getPropertyAsInt("vcc.view.lpanel.width",390),500)
  preferredSize=minimumSize
  val damagePanel=new view.DamageCommandPanel(uia,tracker) 
  val initiativePanel = new view.InitiativePanel(tracker)
  val effectEditorPanel = new view.EffectEditorPanel(tracker)
  
  add(new BoxPanel(Orientation.Vertical) {
    contents+= damagePanel 
    contents+= initiativePanel
    contents+= effectEditorPanel
  },BorderPanel.Position.North)
  
  def changeContext(context:Option[ViewCombatant]) {
    damagePanel.context=context
    initiativePanel.context=context
    effectEditorPanel.context=context
  }
  
  def updateSequence(seq:Seq[ViewCombatant]) {
    initiativePanel.updateSequence(seq)
    effectEditorPanel.updateSequence(seq)
  }
}

