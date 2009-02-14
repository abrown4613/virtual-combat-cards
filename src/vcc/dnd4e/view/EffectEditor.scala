//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing.MigPanel
import scala.actors.Actor

class EffectEditor extends MigPanel("fillx, gap 2 2, ins 0, hidemode 3","","[][][22!]") {
  val smallfont= new java.awt.Font(java.awt.Font.SANS_SERIF,0,10)

  //border= javax.swing.BorderFactory.createTitledBorder("Effect")
  val typeCombo=new ComboBox(List("Any","Mark")) {
    //makeEditable()(ComboBox.stringEditor)
    font=smallfont
  }
  
  val permantMarkCheck = new CheckBox("No supersed")
  
  val descText=new TextField() {
    visible=true
  }
  
  abstract class Duration
  case class Timed(until:Int,of:Symbol) extends Duration
  case class Other() extends Duration
  
  case class DurationComboEntry(text:String,f:()=>Duration) {
    override def toString():String=text
  }
  
  val durationCombo = new ComboBox(
    List(
      DurationComboEntry("End of target next turn",()=>{Timed(10,Symbol(typeCombo.selection.item))}),
      //"End of encounter",
      //DurationComboEntry("Stance",()=>{12}),
      DurationComboEntry("Other",()=>{Other()}))
  ) {
    font=smallfont
  }
  
  
  val combatantSelectCombo= new ComboBox(List("A","10","B")) {
    font=smallfont
  }

  val panel1=new MigPanel("fillx,gap 1 0, ins 0","[]","[24!]") {
    add(new TextField("its me"),"growx, h 22!")
  }	
  val panel2=new MigPanel("gap 1 0, ins 0","[][][]","[24!]") {
    add(new Label(" by "),"gap rel")
    add(combatantSelectCombo,"gap rel")
    add(new CheckBox("cant be superseded"))
    visible=false
  }
  add(new Label("Description"),"h 22!")
  add(typeCombo,"split 2, h 22!")
  add(panel1,"growx")
  add(panel2,"growx")
  add(new Label(""),"wrap")
  //add(tabbed,"growx,span,wrap")
  add(new Label("Duration"))
  add(durationCombo,"split 3")
  add(new CheckBox("Beneficial"))
  add(new CheckBox("Sustain"),"wrap")
  add(new Button("Add"),"skip,split 2")
  add(new Button("Clear"))
  
    
  listenTo(typeCombo.selection,durationCombo.selection)
  reactions+= {
    case event.SelectionChanged(this.durationCombo) =>
      println(durationCombo.selection.item.text +" implies "+ durationCombo.selection.item.f())
    case event.SelectionChanged(this.typeCombo) => 
      typeCombo.selection.item match {
        case "Any" => 
          panel1.visible=true
          panel2.visible=false
        case "Mark" => 
          panel1.visible=false
          panel2.visible=true
      }
    case s => println(s)
  }
  

}

class EffectEditorPanel(tracker: Actor) extends MigPanel("fillx,hidemode 3") {
  
  border= javax.swing.BorderFactory.createTitledBorder("Effect Creation")
  add(new Label("Source:"),"span,split 2")
  add(new ComboBox(List('A,'B,'C)),"wrap,growx")
  addSeparator("Effect")
  add(new EffectEditor(),"span 2,wrap,grow x")
  addSeparator("Effect")
  val efp2=new EffectEditor()
  add(efp2,"span 2,wrap,grow x")
/*
  efp2.visible=false
  listenTo(this.Mouse.moves,this)
  
  reactions += {
    case event.MouseEntered(s,p,i) => 
      efp2.visible=true
      println("Entered at "+p+"size "+this.size); 
    case event.MouseExited(s,p,i) =>
      if(p.x<0 || p.x>=size.getWidth || p.y<0 || p.y>=size.getHeight)
        efp2.visible=false
      println("Exit at "+p+"size "+this.size);
    case event.ComponentResized(c) => println("New size is="+c.size)
  }
  */
}