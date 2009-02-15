//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._
import scala.actors.Actor

abstract class Duration
case class Timed(until:Int,of:Symbol) extends Duration
case class Other() extends Duration

case class DurationComboEntry(text:String,generate: (ViewCombatant,ViewCombatant)=>Duration) {
  override def toString():String=text
}

trait SubPanelComboOption {
  val name:String 
  
  def generateEffect(source:ViewCombatant,target:ViewCombatant):String
  
  override def toString()=name
}

class EffectEditor(parent:EffectEditorPanel) extends MigPanel("fillx, gap 2 2, ins 0, hidemode 3","","[][][22!]") {
  val smallfont= new java.awt.Font(java.awt.Font.SANS_SERIF,0,10)

  val descText=new TextField() {
    visible=true
  }
  
  /**
   * A combo box option that included the infomartion to display and what to 
   * generate as an output
   * @param text To appear on the ComboBox
   * @param f A function form (source,target)=> Duration
   */
  val durationCombo = new ComboBox(
    List(
      DurationComboEntry("End of source's next turn",(s,t)=>{Timed(10,s.id)}),
      DurationComboEntry("End of target's next turn",(s,t)=>{Timed(10,t.id)}),
      DurationComboEntry("End of encounter",(s,t)=>{Timed(12,null)}),
      //DurationComboEntry("Stance",()=>{12}),
      DurationComboEntry("Other",(s,t)=>{Other()}))
  ) {
    font=smallfont
  }
  
  //This is the subPanel for most general case
  val generalSubPanel=new MigPanel("fillx,gap 1 0, ins 0","[]","[24!]") with SubPanelComboOption {
    val name="Any"
    private val descField=new TextField()
    add(descField,"growx, h 22!")
    visible=false
    def generateEffect(source:ViewCombatant,target:ViewCombatant):String ={
      descField.text
    }
  }
  
  // This is the mark paness
  val markSubPanel=new MigPanel("gap 1 0, ins 0","[][][]","[24!]") with SubPanelComboOption {
    
    private val markerText = new ExplicitModelComboBox(parent.idComboModel)//new TextField { columns=4}
    private val permanentMarkCheck= new CheckBox("cant be superseded") 
    val name="Mark"
    add(new Label(" by "),"gap rel")
    add(markerText,"gap rel, w 40!")
    add(permanentMarkCheck)
    visible=false
    listenTo(markerText.selection)
    var i=0
    reactions+= {
      case event.SelectionChanged(this.markerText) =>
        println("update sequence"+markerText.selection.item)
    }
    def generateEffect(source:ViewCombatant,target:ViewCombatant):String ={
      "Mark("+markerText+",perm="+ permanentMarkCheck.selected +")"
    }
  }
  
  val subPanels=List(generalSubPanel,markSubPanel)

  val typeCombo=new ComboBox(subPanels) {
    font=smallfont
  }
  
  val addButton=new Button("Add") { enabled=false }
  val clearButton=new Button("Clear")
  add(new Label("Description"),"h 22!")
  add(typeCombo,"split 2, h 22!")
  for(sp<-subPanels) add(sp,"growx")
  subPanels(0).visible=true
  add(new Label(""),"wrap")
  //add(tabbed,"growx,span,wrap")
  add(new Label("Duration"))
  add(durationCombo,"split 3")
  add(new CheckBox("Beneficial"))
  add(new CheckBox("Sustain"),"wrap")
  add(addButton,"skip,split 2")
  add(clearButton)
  
    
  listenTo(typeCombo.selection,durationCombo.selection)
  listenTo(addButton,clearButton)
  reactions+= {
    case event.SelectionChanged(this.durationCombo) =>
      println(durationCombo.selection.item.text +" implies "+ durationCombo.selection.item.generate)
    case event.SelectionChanged(this.typeCombo) =>
      for(p <- subPanels) { p.visible= p==typeCombo.selection.item }
    case event.ButtonClicked(this.addButton) =>
      parent.createEffect(
        typeCombo.selection.item,
        durationCombo.selection.item)
    //case s => println(s)
  }

  /**
   * Make sure Add button is enabled only with context active
   */
  def setContext(nctx:Option[ViewCombatant]) {
    addButton.enabled=nctx.isDefined
  }
}

