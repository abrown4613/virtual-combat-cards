//$Id$
package vcc.view

import swing._
import swing.event._
import javax.swing.BorderFactory
import util.swing.MigPanel

import vcc.dnd4e.controller.actions._

class DamageCommandPanel(val controller:actors.Actor) extends MigPanel("","[]5[40][fill][fill][fill]","[]10[]") with ContextualView[ViewCombatant]{
  val damage=new TextField {
    columns=3
    enabled=false
  }
  val damage_btn= new Button("Damage")
  val heal_btn= new Button("Heal")
  val temp_btn= new Button("Set Temporary")
  val death_btn = new Button("Fail Death Save")
  val undie_btn = new Button("\"undie\"")
  val controls=List(damage, damage_btn, heal_btn, temp_btn, death_btn,undie_btn)

  contents++ List[Component](new Label("Hit Points:"),damage,damage_btn,heal_btn)
  add(temp_btn,"wrap")
  add(undie_btn,"skip 2, align left")
  add(death_btn,"align left,span 2")
  border=BorderFactory.createTitledBorder("Change Health")
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  for(x<-controls) listenTo(x)
  reactions +={
    case ButtonClicked(this.death_btn) =>
      controller ! FailDeathSave(context.id)

    case ButtonClicked(this.undie_btn) => 
      controller ! Undie(context.id)

    case ButtonClicked(button) => {
      try {
        val value=damage.text.toInt
        if(value != 0 )
          button match {
            case this.damage_btn => controller ! ApplyDamage(context.id, value)
            case this.heal_btn => controller ! HealDamage(context.id,value)
            case this.temp_btn => controller ! SetTemporaryHP(context.id,value)
          }
        damage.text=""
      } catch {
        case nfe:NumberFormatException => println("Bad number")
      }
    }
  }
  changeContext(None)
  
  def changeContext(context:Option[ViewCombatant]) {
    controls map (x=>x.enabled= context!=None)
  }

}
