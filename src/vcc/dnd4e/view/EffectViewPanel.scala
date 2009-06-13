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
import vcc.dnd4e.model.{Effect,Condition}
import vcc.dnd4e.controller.request

class EffectViewPanel(tracker:Actor) extends MigPanel("fillx") with ContextualView[ViewCombatant]
{
  private val sustainButton=new Button("Sustain")
  sustainButton.enabled=false
  private val cancelButton=new Button("Cancel Effect")
  cancelButton.enabled=false
  
  val effectTable=new RowProjectionTable[(Symbol,Int,Effect)]() with CustomRenderedRowProjectionTable[(Symbol,Int,Effect)]{
	val labelFormatter= tabular.EffectTableColorer
    projection=new vcc.util.swing.ProjectionTableModel[(Symbol,Int,Effect)](new tabular.EffectTableProjection(tracker))
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    //model=effectModel
    setColumnWidth(0,25)
    setColumnWidth(1,50,50,100)
    setColumnWidth(2,200)
  }
  
  add(new Label("Active Effects"),"wrap")
  add(new ScrollPane(effectTable),"growy,growprio 50,h 50:150,wrap")
  add(sustainButton,"split 3")
  add(cancelButton)
  
  listenTo(effectTable.selection)
  listenTo(sustainButton,cancelButton)
  
  reactions += {
    case event.ButtonClicked(this.sustainButton) =>
      tracker ! request.SustainEffect(context.id,effectTable.selection.rows.toSeq(0))
    case event.ButtonClicked(this.cancelButton) =>
      tracker ! request.CancelEffect(context.id,effectTable.selection.rows.toSeq(0))
    case event.TableRowsSelected(this.effectTable,rng,opt) =>
      val sel=effectTable.selection.rows
      if(sel.isEmpty) {
        cancelButton.enabled=false
        sustainButton.enabled=false
      } else {
        val eff=effectTable.content(sel.toSeq(0))
        sustainButton.enabled=eff._3.sustainable
        cancelButton.enabled=true
      }
  }
  
  /**
   * Update table according to context
   */
  def changeContext(nctx:Option[ViewCombatant]) {
    nctx match {
      case Some(c) => 
        SwingHelper.invokeLater(()=>{
          effectTable.content= (0 to c.effects.length-1).map(pos=>(c.id,pos,c.effects(pos)))
        })
      case None =>
    }
  }
}
