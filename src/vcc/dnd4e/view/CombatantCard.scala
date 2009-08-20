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
import util.swing._

import scala.actors.Actor
import helper.CombatantStatBlockCache

class CombatantCard(tracker:Actor) extends GridPanel(1,1) with ContextualView[ViewCombatant] {

  minimumSize = new java.awt.Dimension(300,400)
  
  private val effects=new EffectViewPanel(tracker)
  private val commentPanel= new view.CommentPanel(tracker)
  private val statBlock = new XHTMLPane
  statBlock.minimumSize = new java.awt.Dimension(200,200)
  private val split1=new SplitPane(Orientation.Horizontal,effects,commentPanel)
  private val split2=new SplitPane(Orientation.Horizontal,statBlock,split1)
  split1.peer.setDividerSize(4)
  split1.peer.setDividerLocation(0.5)
  split2.peer.setDividerSize(4)
  split2.peer.setDividerLocation(300)
  
  contents += split2
  
  def changeContext(nctx:Option[ViewCombatant]) {
    commentPanel.context=nctx
    effects.context=nctx
    if(nctx.isDefined) statBlock.setDocument(CombatantStatBlockCache.getStatBlockDocumentForCombatant(nctx.get.entity.eid))
    else statBlock.setDocumentFromText("") 
  }
}
