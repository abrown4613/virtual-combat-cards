//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._
import scala.actors.Actor

import vcc.dnd4e.controller.request
import vcc.dnd4e.model.Effect

class EffectEditorPanel(tracker: Actor) extends MigPanel("fillx,hidemode 3") with SequenceView[ViewCombatant] with ContextualView[ViewCombatant]{
  
  private val memory= scala.collection.mutable.Map.empty[String,List[EffectEditor.StateMemento]]
  private var lastActiveKey:String=null
  
  class ActiveCombatant(val c:ViewCombatant) {
    override def toString()= c.id.name + " - "+c.name
  }
  
  private val other=new ActiveCombatant(new ViewCombatant(Symbol("?"),"Terrain or other",0,0,null))
  val activeCombo=new ComboBox[ActiveCombatant](List(other))
  
  private val efpl = List(
    new EffectEditor(this),
    //new EffectEditor(this),
    new EffectEditor(this))
  
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
      switchActive(activeCombo.selection.item.c.name)
/*      println(activeCombo.selection.index )
      println(activeCombo.selection.item.c.name)
      for(efp<-efpl)
    	  println(efp.saveMemento())
  //TEST
  efpl(0).restoreMemento(EffectEditor.StateMemento(0,("I set this"),4,true))
  efpl(1).restoreMemento(EffectEditor.StateMemento(1,('G,true),3,false))
  */
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
    switchActive(activeCombo.selection.item.c.name)
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
}
