//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._
import scala.actors.Actor

import vcc.dnd4e.model.{Effect,Condition}

/**
 * A combo box option that included the infomartion to display and what to 
 * generate as an output
 * @param text To appear on the ComboBox
 * @param generate A function form (source,target)=> Duration
 */
case class DurationComboEntry(text:String,generate: (ViewCombatant,ViewCombatant)=>Effect.Duration) {
  override def toString():String=text
}

trait EffectSubPanelComboOption {
  val name:String 
  
  def generateEffect(source:ViewCombatant,target:ViewCombatant):Condition
  
  def saveMemento():Any
  
  def restoreMemento(memento:Any)
  
  override def toString()=name
}

object EffectEditor {
  case class StateMemento(spIdx:Int,spMemento:Any,durIdx:Int,benef:Boolean)
}

class EffectEditor(parent:EffectEditorPanel) extends MigPanel("fillx, gap 2 2, ins 0, hidemode 3","","[][][22!]") {
  
  private val smallfont= new java.awt.Font(java.awt.Font.SANS_SERIF,0,10)

  private val idComboModel=new ContainterComboBoxModel[String](Nil)
  
  private val durationCombo = new ComboBox(
    List(
      DurationComboEntry("End of source's next turn",(s,t)=>{Effect.Duration.RoundBound(s.id,Effect.Duration.Limit.EndOfNextTurn,false)}),
      DurationComboEntry("End of source's next turn, sustain",(s,t)=>{Effect.Duration.RoundBound(s.id,Effect.Duration.Limit.EndOfNextTurn,true)}),
      DurationComboEntry("Start of source's next turn",(s,t)=>{Effect.Duration.RoundBound(s.id,Effect.Duration.Limit.StartOfNextTurn,false)}),
      DurationComboEntry("End of encounter",(s,t)=>{Effect.Duration.EndOfEncounter}),
      DurationComboEntry("Stance",(s,t)=>{Effect.Duration.Stance}),
      DurationComboEntry("Save End",(s,t)=>{Effect.Duration.SaveEnd}),
      DurationComboEntry("Save End (Special)",(s,t)=>{Effect.Duration.SaveEndSpecial}),
      DurationComboEntry("Other",(s,t)=>{Effect.Duration.Other}),
      DurationComboEntry("End of target's next turn",(s,t)=>{Effect.Duration.RoundBound(t.id,Effect.Duration.Limit.EndOfNextTurn,false)}),
      DurationComboEntry("Start of target's next turn",(s,t)=>{Effect.Duration.RoundBound(t.id,Effect.Duration.Limit.StartOfNextTurn,false)}))
  ) {
    font=smallfont
  }
  
  //This is the subPanel for most general case
  private val generalSubPanel=new MigPanel("fillx,gap 1 0, ins 0","[]","[24!]") with EffectSubPanelComboOption {
    val name="Any"
    private val descField=new TextField()
    add(descField,"growx, h 22!")
    visible=false
    def generateEffect(source:ViewCombatant,target:ViewCombatant):Condition ={
      Condition.Generic(descField.text)
    }
    
    def saveMemento():Any = descField.text
    
    def restoreMemento(memento:Any) {
      memento match {
        case text:String => descField.text=text 
        case _ =>
      }
    }
  }
  
  // This is the mark panel
  private val markSubPanel=new MigPanel("gap 1 0, ins 0","[][][]","[24!]") with EffectSubPanelComboOption {
    private val markerText = new ExplicitModelComboBox(idComboModel)//new TextField { columns=4}
    private val permanentMarkCheck= new CheckBox("cant be superseded") 
    val name="Mark"
    add(new Label(" by "),"gap rel")
    add(markerText,"gap rel, wmin 40")
    add(permanentMarkCheck)
    visible=false

    def generateEffect(source:ViewCombatant,target:ViewCombatant):Condition ={
      Condition.Mark(Symbol(markerText.selection.item),permanentMarkCheck.selected)
    }
    
    def saveMemento():Any= (Symbol(markerText.selection.item),permanentMarkCheck.selected)
    
    def restoreMemento(memento:Any) {
      memento match {
        case (marker:Symbol,perm:Boolean) => 
          val idx=idComboModel.contents.indexOf(marker.name)
          permanentMarkCheck.selected=perm
          markerText.selection.index = idx
          this.repaint
        case _ =>
      }
    }

  }
  
  private val subPanels=List(generalSubPanel,markSubPanel)

  private val typeCombo=new ComboBox(subPanels) {
    font=smallfont
  }
  
  private val addButton=new Button("Add") { enabled=false }
  private val clearButton=new Button("Clear")
  private val benefCheckbox=new CheckBox("Beneficial") {
    tooltip="Check if the effect is beneficial for the target"
  }
  
  add(new Label("Condition"),"h 22!")
  add(typeCombo,"split 2, h 22!")
  for(sp<-subPanels) add(sp,"growx")
  subPanels(0).visible=true
  add(new Label(""),"wrap")
  add(new Label("Duration"))
  add(durationCombo,"split 3")
  add(benefCheckbox,"wrap")
  add(addButton,"skip,split 2")
  add(clearButton)
    
  listenTo(typeCombo.selection)
  listenTo(addButton,clearButton)
  reactions+= {
    case event.SelectionChanged(this.typeCombo) =>
      for(p <- subPanels) { p.visible= p==typeCombo.selection.item }
    case event.ButtonClicked(this.addButton) =>
      parent.createEffect(
        typeCombo.selection.item,
        durationCombo.selection.item,
        benefCheckbox.selected
      )
    case event.ButtonClicked(this.clearButton)=>
      typeCombo.selection.index=0;
      typeCombo.selection.item.restoreMemento((""))
      typeCombo.repaint()
      durationCombo.selection.index=0
      durationCombo.repaint()
  }
  
  def restoreMemento(memento:EffectEditor.StateMemento) {
    typeCombo.selection.index=memento.spIdx
    durationCombo.selection.index=memento.durIdx
    benefCheckbox.selected=memento.benef
    typeCombo.selection.item.restoreMemento(memento.spMemento)
    this.repaint()
  }
  
  def saveMemento():EffectEditor.StateMemento = {
    EffectEditor.StateMemento(
      typeCombo.selection.index,
      typeCombo.selection.item.saveMemento(),
      durationCombo.selection.index,
      benefCheckbox.selected
    )
  }
  
  /**
   * Make sure Add button is enabled only with context active
   */
  def setContext(nctx:Option[ViewCombatant]) {
    addButton.enabled=nctx.isDefined
  }
  
  /**
   * use this method to update the combo models for marked
   */
  def setSequence(seq:Seq[String]) {
    idComboModel.contents=seq
  }
}

