//$Id$
package vcc.dnd4e.view

import swing._
import event._ 
import javax.swing.BorderFactory

class CombatantSummaryView extends GridBagPanel with ContextualView[ViewCombatant]{
  
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

  private final val defInset= new java.awt.Insets(2,2,2,2)
  private final val insetHeader= new java.awt.Insets(2,20,2,10)
  private def makeConstraints(pos:Pair[Int,Int],l:Int):Constraints = {
    var c=new Constraints
    c.gridwidth=l
    c.fill=GridBagPanel.Fill.Horizontal
    //c.weightx=1.0
    c.insets=defInset
    c.grid=pos
    c
  }
  private def addHeaderLabel(text:String, pos:Pair[Int,Int]) {
    var c=new Constraints
    c.gridwidth=1
    c.fill=GridBagPanel.Fill.Horizontal
    //c.weightx=1.0
    c.insets=insetHeader
    c.grid=pos
    this.add(createHeaderLabel(text),c)
  }
  
  private val name=new Label("-")
  name.font=new java.awt.Font(java.awt.Font.SANS_SERIF,java.awt.Font.ITALIC,24)
  
  
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;  
  
  private val name_const=makeConstraints((0,0),5)
  name_const.weightx=1.0
  this.add(name,name_const)
  addHeaderLabel("HP",(0,1))
  this.add(cur_hp,makeConstraints((1,1),1))
  addHeaderLabel("Temp HP",(0,2))
  add(temp_hp,makeConstraints((1,2),1))
  addHeaderLabel("Total",(0,3))
  add(total_hp,makeConstraints((1,3),1))
  addHeaderLabel("Status",(0,4))
  add(status,makeConstraints((1,4),1))
  
  addHeaderLabel("AC",(2,1))
  add(ac_label,(3,1))
  addHeaderLabel("Fortitude",(2,2))
  add(fortitude_label,(3,2))
  addHeaderLabel("Reflex",(2,3))
  add(reflex_label,(3,3))
  addHeaderLabel("Will",(2,4))
  add(will_label,(3,4))

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
