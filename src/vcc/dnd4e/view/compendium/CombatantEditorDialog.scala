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
  
  val f = new Form(null)
  
  val saveButton = new Button(Action("Save & Close") {
    println("Field values: "+f.extractMap)                            
  })
  
  f.setChangeAction(field=>{
    saveButton.enabled = f.isValid 
  })
  
  val fs:List[(String,Field[_])]= List(
    ("Name",combatant.name)) :::
      (combatant match {
        case monster:MonsterEntity => List(
          ("Level",monster.level),
          ("Role",monster.role),
          ("XP",monster.xp)
        )
        case char:CharacterEntity=> List(
          ("Level",char.level),
          ("Race",char.race),
          ("Class",char.charClass)
        )
      }) :::
  List(
    ("Initiative",combatant.initiative),
    ("Hit Points",combatant.hp),
    ("AC",combatant.ac),
    ("Fortitude",combatant.fortitude),
    ("Reflex",combatant.reflex),
    ("Will",combatant.will),
    ("Stat Block",combatant.statblock)
  )
  fs.foreach(t=> new FormTextField(t._1,t._2,f))
  
  val statBlock = new TextArea()
  
  val fc = new MigPanelFormContainter("[50][150,fill][200]")
  f.layout(fc)
  contents = new MigPanel("fill,debug") {
    add(fc,"wrap")
    add(saveButton,"span 3")
  }
}

object TestDialog extends SimpleGUIApplication {
  import vcc.dnd4e.domain.compendium.Compendium
  import vcc.infra.datastore.naming.EntityID
  val m = new MonsterEntity(EntityID.generateRandom)
  m.loadFromMap(Map("stat:hp"->"error","base:name"->"test"))
  val top = new CombatantEditorDialog(m)
}
