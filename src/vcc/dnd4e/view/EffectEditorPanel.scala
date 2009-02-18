//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._
import scala.actors.Actor

import vcc.dnd4e.controller.request
import vcc.dnd4e.model.Effect

class EffectEditorPanel(tracker: Actor) extends MigPanel("fillx,hidemode 3") with SequenceView[ViewCombatant] with ContextualView[ViewCombatant]{
  
  class ActiveCombatant(val c:ViewCombatant) {
    override def toString()= c.id.name + " - "+c.name
  }
  
  private val other=new ActiveCombatant(new ViewCombatant(Symbol("?"),"Terrain or other",0,0,null))
  val activeCombo=new ComboBox[ActiveCombatant](List(other))
  
  private val efp1=new EffectEditor(this)
  private val efp2=new EffectEditor(this)

  
  border= javax.swing.BorderFactory.createTitledBorder("Effect Creation")
  add(new Label("Source:"),"span,split 2")
  add(activeCombo,"wrap,growx")
  addSeparator("Effect")
  add(efp1,"span 2,wrap,grow x")
  addSeparator("Effect")
  add(efp2,"span 2,wrap,grow x")
 
  def updateSequence(seq:Seq[ViewCombatant]):Unit= {
    if(seq.isEmpty) {
      activeCombo.peer.setModel(ComboBox.newConstantModel(List(other)))
      efp1.setSequence(Nil)
      efp2.setSequence(Nil)
    } else {
      var nac=seq.map(c=>{new ActiveCombatant(c)}).toList:::List(other)
      activeCombo.peer.setModel(ComboBox.newConstantModel(nac))
      val lid=seq.map(c=>{c.id.name})
      efp1.setSequence(lid)
      efp2.setSequence(lid)
    }
  }

  def changeContext(nctx:Option[ViewCombatant]) {
    efp1.setContext(nctx)
    efp2.setContext(nctx)
  }
  
  def createEffect(subPanel:EffectSubPanelComboOption,durOption:DurationComboEntry,beneficial:Boolean) {
    val source=activeCombo.selection.item.c
    val cond=subPanel.generateEffect(source,context)
    val duration=durOption.generate(source,context)
    tracker ! request.AddEffect(context.id,Effect(source.id,cond,beneficial,duration))
  }
}
