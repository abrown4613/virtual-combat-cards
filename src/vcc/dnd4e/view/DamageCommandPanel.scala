/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package vcc.dnd4e.view

import swing._
import swing.event._
import javax.swing.BorderFactory
import util.swing.MigPanel

import vcc.dnd4e.controller.actions._

class DamageCommandPanel(val uia:actors.Actor, val controller:actors.Actor) extends MigPanel("","[fill][fill][fill][fill]","") with ContextualView[ViewCombatant]{
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
  undie_btn.tooltip="Use this button to bring a dead combatant back to dying state. This will clear death strikes."
  private val controls=List(damage, damage_btn, heal_btn, temp_btn, death_btn,undie_btn)
  private val damageRelButton=List(damage_btn, heal_btn, temp_btn)
  
  private var damageEquation:helper.DamageParser.Term=null

  add(new Label("Hit Points:"))
  add(damage,"wrap")
  add(damage_btn,"skip 1")
  add(heal_btn)
  add(temp_btn,"wrap")
  add(undie_btn,"skip 1,align left")
  add(death_btn,"align left,span 2")
  border=BorderFactory.createTitledBorder("Change Health")
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  for(x<-controls) listenTo(x)
  listenTo(damage)
  changeContext(None)
  
  reactions +={
    case ValueChanged(this.damage) =>
      damageEquation=try{helper.DamageParser.parseString(damage.text)} catch { case _ => null}
      enableDamageControls(damageEquation!=null)
    case FocusGained(this.damage,other,temporary) =>
      uia ! vcc.dnd4e.view.actor.SetTip("Enter equation with: + - * / and parenthesis and variable: 's' for surge value ; 'b' for bloody value")
      damage.selectAll()
    case FocusLost(this.damage,other,temp) => 
      uia ! vcc.dnd4e.view.actor.SetTip("")
      
    case ButtonClicked(this.death_btn) =>
      controller ! FailDeathSave(context.id)

    case ButtonClicked(this.undie_btn) => 
      controller ! Undie(context.id)

    case ButtonClicked(button) if(damageEquation!=null)=> {
      val cinfo= Map(
        "b" ->context.health.base.totalHP/2,
        "s" ->context.health.base.totalHP/4
      )
      val value=damageEquation.apply(cinfo)
      if(value >= 0 )
    	button match {
    	  case this.damage_btn => controller ! ApplyDamage(context.id, value)
    	  case this.heal_btn => controller ! HealDamage(context.id,value)
    	  case this.temp_btn => controller ! SetTemporaryHP(context.id,value)
        }
    }
  }

  def enableDamageControls(enable:Boolean) {
	damage.background=if(enable) java.awt.Color.white else badColor
	for(x<-damageRelButton) x.enabled=enable
  }
  
  def changeContext(context:Option[ViewCombatant]) {
    controls map (x=>x.enabled= context!=None)
  }

}
