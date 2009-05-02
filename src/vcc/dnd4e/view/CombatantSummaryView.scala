//$Id$
package vcc.dnd4e.view

import swing._
import event._ 
import javax.swing.BorderFactory
import vcc.util.swing.MigPanel

class CombatantSummaryView extends MigPanel("","[65!][65!][65!][65!]","") with ContextualView[ViewCombatant]{
  
  protected def createTextField(text:String,cols:Int):TextField = {
    val tf=new TextField(text)
    tf.columns=cols
    tf.editable=false
    tf.peer.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
    tf
  } 
  
  private def createHeaderLabel(text:String) = {
    val lbl=new Label(text)
    lbl.peer.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT)
    lbl
  }
  
  private val cur_hp=createTextField("0",4)
  private val temp_hp=createTextField("0",3)
  private val status=createTextField("Ok",3)
  private val total_hp=createTextField("0",4)
  
  private val ac_label=createTextField("--",3)
  private val fortitude_label=createTextField("--",3)
  private val reflex_label=createTextField("--",3)
  private val will_label=createTextField("--",3)

  private val name=new Label("-")
  name.font=new java.awt.Font(java.awt.Font.SANS_SERIF,java.awt.Font.ITALIC,20)
  
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;  
  
  this.add(name,"span 4,align center, wrap")
  
  add(new Label("HP"),"align right")
  add(cur_hp)
  add(new Label("AC"),"align right")
  add(ac_label,"wrap")
  
  add( new Label("Total"),"align right")
  add(total_hp)
  add(new Label("Fortitude"),"align right")
  add(fortitude_label,"wrap")
  
  add( new Label("Temp HP"),"align right")
  add(temp_hp)
  add(new Label("Reflex"),"align right")
  add(reflex_label,"wrap")

  add( new Label("Status"),"align right")
  add(status)
  add( new Label("Will"),"align right")
  add(will_label,"wrap")

  border= BorderFactory.createTitledBorder("Combatant Information")

  private def formatDefense(score:Int):String = {
    if(score>0) score.toString else "--"
  }
  
  def changeContext(context:Option[ViewCombatant]) = {
    context match {
      case None => 
        for(x<-List(cur_hp,total_hp,status,temp_hp)) x.text=""
        name.text="-"
      case Some(c)=>
        name.text=c.name
        cur_hp.text=c.health.currentHP.toString
        temp_hp.text=c.health.temporaryHP.toString
        total_hp.text=c.hp.toString
        status.text=c.health.status.toString
        if(c.defense!=null) {
          ac_label.text=formatDefense(c.defense.ac)
          fortitude_label.text=formatDefense(c.defense.fortitude)
          will_label.text=formatDefense(c.defense.will)
          reflex_label.text=formatDefense(c.defense.reflex)
        }
    }
  }
}
