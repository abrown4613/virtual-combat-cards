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
package vcc.dnd4e.view

import dnd.UnifiedCombatantActionTransfer
import scala.swing._
import vcc.util.swing._
import vcc.dnd4e.tracker.common._
import org.slf4j.LoggerFactory
import vcc.util.swing.dnd.{DragAndDropSource, DragAndDropController}

/**
 * A combo box option that included the information to display and what to
 * generate as an output
 * @param text To appear on the ComboBox
 * @param generate A function form (source,target)=> Duration
 */
abstract class DurationComboEntry(text: String) {
  def isDefinedAt(source: UnifiedCombatant, target: UnifiedCombatant): Boolean

  def generate(source: UnifiedCombatant, target: UnifiedCombatant): Duration

  override def toString: String = text
}

class StaticDurationComboEntry(text: String, duration: Duration) extends DurationComboEntry(text) {
  def isDefinedAt(source: UnifiedCombatant, target: UnifiedCombatant): Boolean = true

  def generate(source: UnifiedCombatant, target: UnifiedCombatant): Duration = duration
}

class BoundDurationComboEntry(text: String, limit: Duration.Limit.Value, ofSource: Boolean) extends DurationComboEntry(text) {
  def isDefinedAt(source: UnifiedCombatant, target: UnifiedCombatant): Boolean = if (ofSource) source.isInOrder else target.isInOrder

  def generate(source: UnifiedCombatant, target: UnifiedCombatant): Duration =
    Duration.RoundBound(if (ofSource) source.orderId else target.orderId, limit)
}

trait EffectSubPanelComboOption {
  val panelName: String

  def generateCondition(): Condition

  def saveMemento(): Any

  def restoreMemento(memento: Any)

  override def toString = panelName
}

object EffectEditor {

  case class StateMemento(spIdx: Int, spMemento: Any, durIdx: Int, benef: Boolean)

  private val durations = List(
    new BoundDurationComboEntry("End of source's next turn", Duration.Limit.EndOfNextTurn, true),
    new BoundDurationComboEntry("End of source's next turn, sustain", Duration.Limit.EndOfNextTurnSustain, true),
    new BoundDurationComboEntry("Start of source's next turn", Duration.Limit.StartOfNextTurn, true),
    new StaticDurationComboEntry("End of Encounter", Duration.EndOfEncounter),
    new StaticDurationComboEntry("Stance", Duration.Stance),
    new StaticDurationComboEntry("Rage", Duration.Rage),
    new StaticDurationComboEntry("Save End", Duration.SaveEnd),
    new StaticDurationComboEntry("Save End (Special)", Duration.SaveEndSpecial),
    new StaticDurationComboEntry("Other", Duration.Other),
    new BoundDurationComboEntry("End of target's next turn", Duration.Limit.EndOfNextTurn, false),
    new BoundDurationComboEntry("Start of target's next turn", Duration.Limit.StartOfNextTurn, false)
  )

  val dictionary: AutoCompleteDictionary = {
    val logger = LoggerFactory.getLogger("startup")
    val resource = this.getClass.getResource("/vcc/dnd4e/view/autocomplete.dict")
    if (resource != null) {
      try {
        AutoCompleteDictionary.fromStream(resource.openStream(), (term, msg) => {
          logger.warn("AutoComplete[{}]: {}", term, msg)
        })
      } catch {
        case e =>
          logger.warn("Failed to open resource: /vcc/dnd4e/view/autocomplete.dict", e)
          new AutoCompleteDictionary(Nil)
      }
    } else {
      new AutoCompleteDictionary(Nil)
    }
  }

  class GeneralConditionPanel(benefCheckbox: CheckBox, parent: EffectEditorPanel) extends MigPanel("fillx,gap 1 0, ins 0", "[]", "[24!]") with EffectSubPanelComboOption {
    val panelName = "Any"
    private val descField = new TextField() with AutoCompleteTextComponent
    descField.enableAutoComplete(EffectEditor.dictionary)
    descField.tooltip = "Enter condition text. Hit ENTER to accept the suggested completion."
    add(descField, "growx, h 22!")
    visible = false

    def generateCondition(): Condition = {
      Effect.Condition.Generic(descField.text, benefCheckbox.selected)
    }

    def saveMemento(): Any = (descField.text, benefCheckbox.selected)

    listenTo(descField)
    reactions += {
      case event.FocusGained(`descField`, _, _) =>
        parent.setStatusBarMessage(descField.tooltip)
      case event.FocusLost(`descField`, _, _) =>
        parent.setStatusBarMessage("")
    }

    def restoreMemento(memento: Any) {
      memento match {
        case (text: String, benef: Boolean) =>
          descField.text = text
          benefCheckbox.selected = benef
        case _ =>
      }
    }
  }

  class MarkConditionPanel(model: ContainerComboBoxModel[CombatantID]) extends MigPanel("gap 1 0, ins 0", "[][][]", "[24!]") with EffectSubPanelComboOption {
    private val markerText = new ExplicitModelComboBox[CombatantID](model)
    markerText.setFormatRenderer(new StringFormatListCellRenderer[CombatantID](cid => cid.id))
    private val permanentMarkCheck = new CheckBox("cant be superseded")
    val panelName = "Mark"
    add(new Label(" by "), "gap rel")
    add(markerText, "gap rel, wmin 40")
    add(permanentMarkCheck)
    visible = false

    def generateCondition(): Condition = {
      Effect.Condition.Mark(markerText.selection.item, permanentMarkCheck.selected)
    }

    def saveMemento(): Any = (markerText.selection.item, permanentMarkCheck.selected)

    def restoreMemento(memento: Any) {
      memento match {
        case (marker: CombatantID, perm: Boolean) =>
          val idx = model.contents.indexOf(marker)
          permanentMarkCheck.selected = perm
          markerText.selection.index = idx
          this.repaint()
        case _ =>
      }
    }
  }

}

class EffectEditor(parent: EffectEditorPanel) extends MigPanel("fillx, gap 2 2, ins 0, hidemode 3", "", "[][][22!]") {
  private var source: Option[UnifiedCombatant] = None
  private var target: Option[UnifiedCombatant] = None

  private val smallFont = new java.awt.Font(java.awt.Font.SANS_SERIF, 0, 10)

  private val idComboModel = new ContainerComboBoxModel[CombatantID](Nil)

  private val durationCombo = new ComboBox[DurationComboEntry](EffectEditor.durations) {
    font = smallFont
  }
  private val benefCheckbox = new CheckBox("Beneficial")
  benefCheckbox.tooltip = "Check if the effect is beneficial for the target"

  //This is the subPanel for most general case
  private val generalSubPanel = new EffectEditor.GeneralConditionPanel(benefCheckbox, parent)

  // This is the mark panel
  private val markSubPanel = new EffectEditor.MarkConditionPanel(idComboModel)
  private val subPanels = List[Panel with EffectSubPanelComboOption](generalSubPanel, markSubPanel)

  private val typeCombo = new ComboBox[Panel with EffectSubPanelComboOption](subPanels)
  typeCombo.font = smallFont

  private val addButton = new Button("Add")
  addButton.enabled = false
  addButton.tooltip = "Click to add effect to select target, or drag to any valid target"

  private val clearButton = new Button("Clear")

  add(new Label("Condition"), "h 22!")
  add(typeCombo, "split 2, h 22!")
  for (sp <- subPanels) add(sp, "growx")
  subPanels(0).visible = true
  add(new Label(""), "wrap")
  add(new Label("Duration"))
  add(durationCombo, "split 3")
  add(benefCheckbox, "wrap")
  add(addButton, "skip,split 2")
  add(clearButton)

  private val dc = new DragAndDropController(IconLibrary.MiniWand)
  dc.enableDragToCopy(DragAndDropSource.fromButton(addButton, () => {
    val durationBuilder = durationCombo.selection.item
    val condition = typeCombo.selection.item.generateCondition()
    val beneficial = benefCheckbox.selected
    val src = source.get

    new UnifiedCombatantActionTransfer(
    condition.description, {
      case tgt if (durationBuilder.isDefinedAt(src, tgt)) =>
        parent.createEffect(tgt, condition, durationBuilder.generate(src, tgt), beneficial)
        false
    }).toTransferable
  }))

  listenTo(typeCombo.selection)
  listenTo(durationCombo.selection)
  listenTo(addButton, clearButton)
  reactions += {
    case event.SelectionChanged(this.typeCombo) =>
      for (p <- subPanels) {
        p.visible = (p equals typeCombo.selection.item)
      }
    case event.SelectionChanged(this.durationCombo) =>
      checkAddButton()
    case event.ButtonClicked(this.addButton) =>
      parent.createEffect(
        parent.getTargetCombatant,
        typeCombo.selection.item.generateCondition(),
        durationCombo.selection.item.generate(source.get, parent.getTargetCombatant),
        benefCheckbox.selected)
    case event.ButtonClicked(this.clearButton) =>
      clearPanel()
  }

  def restoreMemento(memento: EffectEditor.StateMemento) {
    typeCombo.selection.index = memento.spIdx
    durationCombo.selection.index = memento.durIdx
    benefCheckbox.selected = memento.benef
    typeCombo.selection.item.restoreMemento(memento.spMemento)
    this.repaint()
  }

  def saveMemento(): EffectEditor.StateMemento = {
    EffectEditor.StateMemento(
      typeCombo.selection.index,
      typeCombo.selection.item.saveMemento(),
      durationCombo.selection.index,
      benefCheckbox.selected
    )
  }

  def clearPanel() {
    typeCombo.selection.index = 0;
    typeCombo.selection.item.restoreMemento(("", false))
    typeCombo.repaint()
    durationCombo.selection.index = 0
    durationCombo.repaint()
  }

  /**
   * Make sure Add button is enabled only with context active
   */
  def setContext(nctx: Option[UnifiedCombatant], isTarget: Boolean) {
    if (isTarget) target = nctx
    else source = nctx
    checkAddButton()
  }

  /**
   * use this method to update the combo models for marked
   */
  def setSequence(seq: Seq[CombatantID]) {
    idComboModel.contents = seq
  }

  private def checkAddButton() {
    if (source.isDefined && target.isDefined)
      addButton.enabled = durationCombo.selection.item.isDefinedAt(source.get, target.get)
    else
      addButton.enabled = false
  }

}

