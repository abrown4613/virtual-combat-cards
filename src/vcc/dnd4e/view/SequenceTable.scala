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
import vcc.util.swing._
import scala.actors.Actor

class SequenceTable(uia:Actor,tracker:Actor) extends ScrollPane with ContextualView[ViewCombatant] with SequenceView[ViewCombatant] {
  //Init
  val table = new RowProjectionTable[ViewCombatant] with CustomRenderedRowProjectionTable[ViewCombatant] with KeystrokeActionable {
    val labelFormatter=ViewCombatantTableColorer
    projection = new vcc.util.swing.ProjectionTableModel[ViewCombatant](view.ViewCombatantProjection)
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    setColumnWidth(0,25)
    setColumnWidth(1,150)
    setColumnWidth(2,70)
    setColumnWidth(3,70)
    setColumnWidth(4,50)
    setColumnWidth(5,70)
    peer.setRowHeight(24)
  }
  
  this.contents=table
  val setAction=Action("SetActing") {
    if(isInContext) uia ! actor.SetActing(context.id)
  }
  table.bindKeystrokeAction(FocusCondition.WhenAncestorFocused,"alt A", setAction)
  //table.peer.getInputMap(javax.swing.JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(javax.swing.KeyStroke.getKeyStroke("ctrl pressed A"),"SetActing")
  table.bindKeystrokeAction("alt M", Action("table.mark") {
    if(isInContext) {
      val r = uia !? actor.QueryInfo('ACTING)
      r match {
        case Some(c:ViewCombatant) => {
          import vcc.dnd4e.model._
          tracker ! vcc.dnd4e.controller.request.AddEffect(context.id, 
            Effect(c.id, Condition.Mark(c.id,false),false,
            		Effect.Duration.RoundBound(c.id,Effect.Duration.Limit.EndOfNextTurn,false)))
          }
        case s => // Do notthing
      }
    }
  })
  
  var _doingContextChange=false
  
  listenTo(table.selection,table)

  reactions += {
    case TableRowsSelected(t,rng,false) => 
      var l=table.selection.rows.toSeq
      if(!l.isEmpty && !_doingContextChange) {
        var c=table.content(l(0))
        uia ! actor.SetContext(c.id)
      }
    case FocusGained(this.table,other,temporary) =>
      uia ! vcc.dnd4e.view.actor.SetTip("Alt+A to set source on effect panel; Alt+M mark selected combatant")
    case FocusLost(this.table,other,temp) => 
      uia ! vcc.dnd4e.view.actor.SetTip("")
  }
  
  def updateSequence(seq:Seq[ViewCombatant]):Unit = { 
    table.content=seq
    SwingHelper.invokeLater{
      table.selection.rows.clear
      if(table.content.isEmpty) 
        uia ! actor.SetContext(null)
      else
        table.selection.rows+=0
    }
  }

  def changeContext(ctx:Option[ViewCombatant]) {
    if(ctx.isDefined && !table.content.isEmpty && (ctx.get == table.content(0))) {
      _doingContextChange=true;
      table.selection.rows.clear; 
      table.selection.rows+=0
      _doingContextChange=false                                                                 
    }
  }

  /**
   * Schedule a fireTableRowsUpdated() for a later time. Used to refresh the 
   * values in the table.
   */
  def fireUpdate() {
    SwingHelper.invokeLater {
      _doingContextChange=true;
      table.projection.fireTableRowsUpdated(0,table.content.length-1)
      _doingContextChange=false                                                                 
    }
  }
  
}
