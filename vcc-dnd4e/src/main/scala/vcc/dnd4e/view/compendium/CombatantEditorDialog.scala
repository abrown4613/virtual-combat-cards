/**
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing.{MigPanel, XHTMLEditorPane}
import vcc.util.swing.forms._

import vcc.dnd4e.domain.compendium._
import vcc.infra.fields.Field
import vcc.domain.dndi.CaptureTemplateEngine
import vcc.infra.xtemplate.MapDataSource

class CombatantEditorDialog(combatant: CombatantEntity) extends Frame {
  title = "Edit Combatant: " + (if (combatant.name.isValid) combatant.name.storageString else "")

  iconImage = vcc.dnd4e.view.IconLibrary.MetalD20.getImage

  peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
  listenTo(this)
  reactions += {
    case WindowClosing(win) =>
      if (Dialog.showConfirmation(tabPane, "Are you sure you want to exit without saving the changes to the combatant?", "Exit without saving?", Dialog.Options.YesNo) == Dialog.Result.Yes) {
        this.dispose
      }
  }

  private val form = new Form(null)

  private val saveButton: Button = new Button(Action("Save & Close") {
    combatant.loadFromMap(form.extractMap + ("text:statblock" -> statBlock.text))
    if (combatant.isValid) {
      if (Compendium.activeRepository.store(combatant)) {
        // Ok
      } else {
        Dialog.showMessage(saveButton, "Failed to save entity to the repository.", "Save failed", Dialog.Message.Error, null)
      }
      this.visible = false
      this.dispose()
    }
  })
  private val closeButton = new Button(Action("Close") {
    this.dispose()
  })

  form.setChangeAction(field => {
    saveButton.enabled = form.isValid
    generateAction.enabled = form.isValid
    if (field.id == combatant.name.id) title = "Edit Combatant: " + field.storageString
  })

  private val fs: List[(String, Field[_])] = List(
    ("Name", combatant.name)) :::
    (combatant match {
      case trap: TrapEntity => List(
        ("Level", trap.level),
        ("Type", trap.trapClass),
        ("Role", trap.role),
        ("XP", trap.xp)
      )
      case monster: MonsterEntity => List(
        ("Level", monster.level),
        ("Role", monster.role),
        ("XP", monster.xp)
      )
      case char: CharacterEntity => List(
        ("Level", char.level),
        ("Race", char.race),
        ("Class", char.charClass)
      )
    }) :::
    List(
      ("Initiative", combatant.initiative),
      ("Hit Points", combatant.hp),
      ("AC", combatant.ac),
      ("Fortitude", combatant.fortitude),
      ("Reflex", combatant.reflex),
      ("Will", combatant.will)) :::
    (combatant match {
      case char: CharacterEntity => List(
        ("Insight", char.insight),
        ("Perception", char.perception),
        ("Senses", char.senses)
      )
      case _ => Nil
    }) :::
    List(("Comment", combatant.comment))

  fs.foreach(t => new FormTextField(t._1, t._2, form))

  private val generateAction = Action("Generate") {
    val defined = statBlock.text.length > 1
    if ((defined && Dialog.showConfirmation(statBlock, "This action will generate a minimal stat block containing information you have inputed.\n If this is an imported creature, this will lead to loss of information. \nAre you sure?", "Overwrite current statblock", Dialog.Options.YesNo) == Dialog.Result.Yes) || !defined) {
      val template = CaptureTemplateEngine.fetchClassTemplate(combatant.classID.shortClassName)
      statBlock.text = template.render(new MapDataSource(form.extractMap, Map(), Map())).toString
      statBlock.sync()
    }
  }
  private val statBlock: XHTMLEditorPane = new XHTMLEditorPane(combatant.statblock.storageString, generateAction)
  private val fc = new MigPanelFormContainter("[50][200,fill][250]")

  form.layout(fc)
  generateAction.enabled = form.isValid

  private val tabPane = new TabbedPane {
    pages += new TabbedPane.Page("Data", fc)
    pages += new TabbedPane.Page("Stat Block", statBlock)
  }

  contents = new MigPanel("fill") {
    add(tabPane, "wrap,growy,growx")
    add(saveButton, "span 3,split 3")
    add(closeButton)
  }
}