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
import vcc.util.swing.{MigPanel,XHTMLEditorPane}
import vcc.util.swing.forms._

import vcc.dnd4e.domain.compendium._
import vcc.infra.fields.Field

class CombatantEditorDialog(combatant:CombatantEntity) extends Frame {
  
  title = "Edit Combatant: " + (if(combatant.name.isValid) combatant.name.storageString else "")
  
  iconImage = vcc.dnd4e.view.IconLibrary.MetalD20.getImage
  
  private val f = new Form(null)
  
  private val saveButton:Button = new Button(Action("Save & Close") {
    combatant.loadFromMap(f.extractMap + ( "text:statblock" -> statBlock.text) )
    if(combatant.isValid) {
      if(Compendium.activeRepository.store(combatant)) {
        // Ok
      } else {
        Dialog.showMessage(saveButton,"Failed to save entity to the repository.","Save failed",Dialog.Message.Error,null)
      }
      this.visible = false
      this.dispose()
    }
  })
  
  f.setChangeAction(field=>{
    saveButton.enabled = f.isValid
    if(field.id == combatant.name.id) title = "Edit Combatant: "+field.storageString
  })
  
  private val fs:List[(String,Field[_])]= List(
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
        ("Will",combatant.will)) :::
      (combatant match {
        case char:CharacterEntity=> List(
          ("Insight",char.insight),
          ("Perception",char.perception),
          ("Senses",char.senses)
        )
        case _ => Nil
        })    

  fs.foreach(t=> new FormTextField(t._1,t._2,f))
  
  private val statBlock:XHTMLEditorPane = new XHTMLEditorPane(combatant.statblock.storageString, Action("Generate"){
	statBlock.text = SimpleStatBlockBuilder.generate(new FormFieldStatBlockSource(f)).toString
	statBlock.sync()
  })
  private val fc = new MigPanelFormContainter("[50][200,fill][250]")
  
  f.layout(fc)
  
  private val tabPane = new TabbedPane {
    pages += new TabbedPane.Page("Data",fc)
    pages += new TabbedPane.Page("Stat Block", statBlock)
  }
  
  contents = new MigPanel("fill") {
    add(tabPane,"wrap,growy,growx")
    add(saveButton,"span 3")
  }
}
