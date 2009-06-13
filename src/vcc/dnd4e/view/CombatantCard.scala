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

class CombatantCard(tracker:Actor) 
  extends MigPanel("flowy,fill","[300!]","[c,pref!][c,fill]")
  with ContextualView[ViewCombatant]
{

  private val summary=new CombatantSummaryView()
  private val effects=new EffectViewPanel(tracker)
  private val commentPanel= new view.CommentPanel(tracker)
  private val split1=new SplitPane(Orientation.Horizontal,effects,commentPanel)
  
  add(summary,"growx")
  add(split1,"growx")
  
  def changeContext(nctx:Option[ViewCombatant]) {
    summary.context=nctx
    commentPanel.context=nctx
    effects.context=nctx
  }
}
