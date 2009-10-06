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

import vcc.dnd4e.controller.request
import vcc.dnd4e.model.{Effect,CombatantEntity,MinionHealthDefinition,CombatantType}
import vcc.dnd4e.BootStrap

class EffectEditorPanel(uia: Actor,tracker: Actor) extends MigPanel("fillx,hidemode 3") with SequenceView[ViewCombatant] with ContextualView[ViewCombatant]{
  
  private val memory= scala.collection.mutable.Map.empty[String,List[EffectEditor.StateMemento]]
  private var lastActiveKey:String=null
  private var _changing:Boolean = false
  
  case class ActiveCombatant(val c:ViewCombatant) {
    override def toString()= c.id.name + " - "+c.entity.name
  }
  
  val terrainCombatant = CombatantEntity(null,"Terrain or Other",MinionHealthDefinition(),0,CombatantType.Minion,null)
  
  private val other=new ActiveCombatant(new ViewCombatant(Symbol("?"),null,terrainCombatant))
  val activeCombo=new ComboBox[ActiveCombatant](List(other))
  private val efpl = if(
    java.awt.Toolkit.getDefaultToolkit.getScreenSize().getHeight()>700 &&
    BootStrap.getPropertyAsInt("vcc.view.efp.max",3)>2
  ) {
	List(new EffectEditor(this),new EffectEditor(this),new EffectEditor(this))
  } else {
    List(new EffectEditor(this),new EffectEditor(this))
  }
  
  border= javax.swing.BorderFactory.createTitledBorder("Effect Creation")
  add(new Label("Source:"),"span,split 2")
  add(activeCombo,"wrap,growx")
  for(efp<-efpl) {
	  addSeparator("Effect")
	  add(efp,"span 2,wrap,grow x")
  }

  // Set and handle reaction to changing the active characters
  listenTo(activeCombo.selection)
  reactions += {
    case event.SelectionChanged(`activeCombo`)=>
      if(!_changing) uia ! actor.SetActing(activeCombo.selection.item.c.id)
      switchActive(activeCombo.selection.item.c.entity.name)
  }
  
  def updateSequence(seq:Seq[ViewCombatant]):Unit= {
    if(seq.isEmpty) {
      activeCombo.peer.setModel(ComboBox.newConstantModel(List(other)))
      for(efp<-efpl) efp.setSequence(Nil)
    } else {
      var nac=seq.map(c=>{new ActiveCombatant(c)}).toList:::List(other)
      activeCombo.peer.setModel(ComboBox.newConstantModel(nac))
      val lid=seq.map(c=>{c.id.name})
      for(efp<-efpl) efp.setSequence(lid)
    }
    //switchActive(activeCombo.selection.item.c.name)
    uia ! actor.SetActing(activeCombo.selection.item.c.id)
  }

  def changeContext(nctx:Option[ViewCombatant]) {
    for(efp<-efpl) efp.setContext(nctx)
  }
  
  def createEffect(subPanel:EffectSubPanelComboOption,durOption:DurationComboEntry,beneficial:Boolean) {
    val source=activeCombo.selection.item.c
    val cond=subPanel.generateEffect(source,context)
    val duration=durOption.generate(source,context)
    tracker ! request.AddEffect(context.id,Effect(source.id,cond,beneficial,duration))
  }
  
  /**
   * Store data on the effect memory
   */
  def switchActive(nkey:String) {
    // If we have a key store it
    if(lastActiveKey!=null) {
      memory(lastActiveKey)=efpl.map(epl=>epl.saveMemento())
    }
    lastActiveKey=nkey
    // Restore the previous mementos if they exit
    if(memory.contains(nkey)) {
      efpl.zip(memory(nkey)).map(x=>x._1.restoreMemento(x._2))
    }
  }
  
  def setActing(act:Option[ViewCombatant]) {
    if(act.isDefined) {
      _changing=true
      activeCombo.selection.item= new ActiveCombatant(act.get)
      activeCombo.repaint
      _changing=false
    }
  }

}
