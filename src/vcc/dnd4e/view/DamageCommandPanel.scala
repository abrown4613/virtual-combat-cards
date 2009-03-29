//$Id$
package vcc.dnd4e.view

import swing._
import swing.event._
import javax.swing.BorderFactory
import util.swing.MigPanel

import vcc.dnd4e.controller.actions._

class DamageCommandPanel(val controller:actors.Actor) extends MigPanel("","[]5[40][fill][fill][fill]","[]10[]") with ContextualView[ViewCombatant]{
  private val damage=new TextField {
    columns=3
    enabled=false
  }
  private val badColor= new java.awt.Color(255,228,196)
  private val damage_btn= new Button("Damage")
  damage.background=badColor
  private val heal_btn= new Button("Heal")
  private val temp_btn= new Button("Set Temporary")
  private val death_btn = new Button("Fail Death Save")
  private val undie_btn = new Button("\"undie\"")
  private val controls=List(damage, damage_btn, heal_btn, temp_btn, death_btn,undie_btn)
  private val damageRelButton=List(damage_btn, heal_btn, temp_btn)

  contents++ List[Component](new Label("Hit Points:"),damage,damage_btn,heal_btn)
  add(temp_btn,"wrap")
  add(undie_btn,"skip 2, align left")
  add(death_btn,"align left,span 2")
  border=BorderFactory.createTitledBorder("Change Health")
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  for(x<-controls) listenTo(x)
  listenTo(damage)
  changeContext(None)
  
  reactions +={
    case ValueChanged(this.damage) =>
      try {
        damage.text.toInt
        damage.background=java.awt.Color.white
        for(x<-damageRelButton) x.enabled=true
      } catch {
        case nfe:NumberFormatException =>
          damage.background=badColor
          println("Bad number")
          for(x<-damageRelButton) x.enabled=false
      }      
    
    case FocusGained(this.damage,other,temporary) =>
      damage.selectAll()
      
    case ButtonClicked(this.death_btn) =>
      controller ! FailDeathSave(context.id)

    case ButtonClicked(this.undie_btn) => 
      controller ! Undie(context.id)

    case ButtonClicked(button) => {
      val value = try { damage.text.toInt } catch {  case nfe:NumberFormatException => 0 }
      if(value != 0 )
    	button match {
    	  case this.damage_btn => controller ! ApplyDamage(context.id, value)
    	  case this.heal_btn => controller ! HealDamage(context.id,value)
    	  case this.temp_btn => controller ! SetTemporaryHP(context.id,value)
        }
    }
  }

  def changeContext(context:Option[ViewCombatant]) {
    controls map (x=>x.enabled= context!=None)
  }

}
