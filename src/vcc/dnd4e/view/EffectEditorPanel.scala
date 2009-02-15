//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._
import scala.actors.Actor

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
  
  def createEffect(subPanel:EffectSubPanelComboOption,durOption:DurationComboEntry,sustain:Boolean, beneficial:Boolean) {
    val source=activeCombo.selection.item.c
    println("You clicked add to duration of:"+durOption.generate(source,context))
    println("Effect: "+subPanel.generateEffect(source,context))
    println("Benef: "+beneficial)
    println("Sustain: "+sustain)
  }
}
