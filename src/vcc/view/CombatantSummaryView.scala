package vcc.view

import swing._
import event._ 
import javax.swing.BorderFactory

class CombatantSummaryView extends GridBagPanel with ContextualView[ViewCombatant]{
  val cur_hp=new TextField {
    text="0"
    columns=4
    editable=false
  }
  val temp_hp=new TextField {
    text="0"
    columns=3
    editable=false
  }
  val status= new TextField { 
    text="Ok"
    editable=false
    minimumSize=new java.awt.Dimension(20,100)
    size=minimumSize
  }
  val total_hp=new TextField {
    text="0"
    columns=4
    editable=false
  }
  
  private def makeConstraints(pos:Pair[int,int],l:Int):Constraints = {
    var c=new Constraints
    c.gridwidth=l
    c.fill=GridBagPanel.Fill.Horizontal
    c.insets=new java.awt.Insets(2,2,2,2)
    c.grid=pos
    c
  }
  
  val name=new Label("")
  name.font=new java.awt.Font(java.awt.Font.SANS_SERIF,java.awt.Font.ITALIC,24)
 
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;  
  
  this.add(name,makeConstraints((0,0),4))
  this.add(new Label("HP"), (0,1))
  this.add(cur_hp,makeConstraints((0,2),1))
  add(new Label("Temp HP"),(1,1))
  add(temp_hp,makeConstraints((1,2),1))
  add(new Label("Total"),(2,1))
  add(total_hp,makeConstraints((2,2),1))
  add(new Label("Status"),(0,3))
  add(status,makeConstraints((1,3),3))

  border= BorderFactory.createTitledBorder("Combatant Status")

  def changeContext(context:Option[ViewCombatant]) = {
    context match {
      case None => 
        for(x<-List(cur_hp,total_hp,status,temp_hp)) x.text=""
        name.text=""
      case Some(c)=>
        name.text=c.name
        cur_hp.text=c.health.currhp.toString
        temp_hp.text=c.health.temphp.toString
        total_hp.text=c.hp.toString
        status.text=c.health.status.toString
    }
  }
}
