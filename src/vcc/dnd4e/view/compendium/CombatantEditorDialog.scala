//$Id$
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

package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing.MigPanel
import vcc.util.swing.forms._

import vcc.dnd4e.domain.compendium._
import vcc.infra.fields.Field

class CombatantEditorDialog(combatant:CombatantEntity) extends Frame {
  title = "Edit Combatant"
  
  combatant.name.value = "Hello"
  val f = new Form(null)
  
  val saveButton = new Button(Action("Save & Close") {
    println(f.extractMap)                            
  })
  
  f.setChangeAction(field=>{
    println(field.id+"Form changed")
    saveButton.enabled = f.isValid 
  })
  
  //new FormTextField("Name",combatant.name.id,combatant.name.fieldValue,f,combatant.name.validator)
  //new FormTextField("HP",combatant.hp.id,combatant.hp.fieldValue,f,combatant.hp.validator)
  val fs:List[(String,Field[_])]= List(
    ("Name",combatant.name),
    ("Hit Points",combatant.hp)
  )
  fs.foreach(t=> new FormTextField(t._1,t._2,f))
  
  
  val mp = new MigPanel("fill,debug")
  f.layout(mp)
  mp.add(saveButton,"span 3")
  contents = mp
}

object TestDialog extends SimpleGUIApplication {
  import vcc.dnd4e.domain.compendium.Compendium
  import vcc.infra.datastore.naming.EntityID
  val m = new MonsterEntity(EntityID.generateRandom)
  m.loadFromMap(Map("stat:hp"->"error","base:name"->"test"))
  val top = new CombatantEditorDialog(m)
}
