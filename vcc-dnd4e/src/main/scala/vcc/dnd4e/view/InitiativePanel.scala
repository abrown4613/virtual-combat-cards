/*
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
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.Command._
import vcc.infra.docking._
import vcc.dnd4e.domain.tracker.snapshot.StateChange
import vcc.util.swing._

class InitiativePanel(director: PanelDirector) extends MigPanel("flowx,ins 2,hidemode 3", "[19%,fill][27%,fill][27%,fill][27%,fill]", "")
with CombatStateObserver with ContextObserver with ScalaDockableComponent with KeystrokeContainer {
  private val next_btn = new Button("Next")
  next_btn.tooltip = ("End round of the first combatant and start next (shortcut Alt-N)")

  private val delay_btn = new Button("Delay")
  delay_btn.tooltip = "Delay the round of the first combatant"
  private val ready_btn = new Button("Ready")
  ready_btn.tooltip = "Ready an action for the first combatant"
  private val executeReady_btn = new Button("Execute ready")
  executeReady_btn.tooltip = "Make a combatant that is ready execute its action, first combatant needs to be acting"
  private val moveBefore_btn = new Button("Move")
  private val moveLabel = new Label("Move select combatant before:")
  moveLabel.horizontalAlignment = scala.swing.Alignment.Right
  private val firstLabel = new Label("First can:")
  firstLabel.horizontalAlignment = scala.swing.Alignment.Right
  private val targetLabel = new Label("Target can:")
  targetLabel.horizontalAlignment = scala.swing.Alignment.Right
  private val candidateBefore = new ContainerComboBoxModel[InitiativeOrderID](Nil)
  private val before_Combo = new ExplicitModelComboBox[InitiativeOrderID](candidateBefore)
  before_Combo.setFormatRenderer(new StringFormatListCellRenderer[InitiativeOrderID](o => o.toLabelString))

  moveBefore_btn.tooltip = "Move select combatant to a position before the combatant selected on the combo box to the left"

  private var target: Option[UnifiedCombatant] = None
  private var _first: Option[UnifiedCombatant] = None
  private var combatState = director.currentState

  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT;
  add(firstLabel, "align right")
  add(next_btn, "")
  add(delay_btn, "")
  add(ready_btn, "wrap")
  add(targetLabel, "align right")
  add(executeReady_btn, "span 3, growx 0, wrap")
  add(moveLabel, "align right,span 2")
  add(before_Combo, "split 2")
  add(moveBefore_btn)

  listenTo(next_btn, delay_btn, ready_btn, moveBefore_btn, executeReady_btn)

  updatePanel()

  reactions += {
    case ButtonClicked(this.next_btn) if (_first.isDefined) =>
      director requestAction ExecuteInitiativeAction(_first.get.orderId, InitiativeAction.EndRound)
    case ButtonClicked(this.ready_btn) if (_first.isDefined) =>
      director requestAction ExecuteInitiativeAction(_first.get.orderId, InitiativeAction.ReadyAction)
    case ButtonClicked(this.delay_btn) if (_first.isDefined) =>
      director requestAction ExecuteInitiativeAction(_first.get.orderId, InitiativeAction.DelayAction)
    case ButtonClicked(this.executeReady_btn) =>
      director requestAction ExecuteInitiativeAction(target.get.orderId, InitiativeAction.ExecuteReady)
    case ButtonClicked(this.moveBefore_btn) if (before_Combo.selection.item != null) =>
      director requestAction MoveBefore(target.get.orderId, before_Combo.selection.item)
  }

  def changeContext(newContext: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (isTarget) {
      target = combatState.combatantOption(newContext)
    }
    if (newContext.isDefined && isTarget) updatePanel()
  }

  val rules = director.rules

  private def updateMoveBeforeCombo(comb: UnifiedCombatant) {
    val before: Seq[InitiativeOrderID] = if (comb.isInOrder) {
      combatState.elements.filter(c => rules.canMoveBefore(combatState.state, comb.orderId, c.orderId)).map {
        c => c.orderId
      }
    } else {
      Seq()
    }
    candidateBefore.contents = before
    moveLabel.enabled = !before.isEmpty
    moveBefore_btn.enabled = !before.isEmpty
    if(!before.isEmpty) before_Combo.selection.index = 0
    before_Combo.enabled = true
  }

  private def updateLabels() {
    val selectedId: String = if (target.isDefined && target.get.isInOrder) target.get.orderId.toLabelString else null
    firstLabel.text = if (_first.isDefined && _first.get.isInOrder) "[" + _first.get.orderId.toLabelString + "] can:" else "First can:"
    targetLabel.text = if (selectedId != null) "[" + selectedId + "] can:" else "Target can:"
    moveLabel.text = if (selectedId != null) "Move [ " + selectedId + " ] before:" else "Move target before:"
  }

  private def updatePanel() {
    if (_first.isDefined) {
      val firstOrderId = _first.get.orderId
      next_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, firstOrderId, InitiativeAction.EndRound)
      delay_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, firstOrderId, InitiativeAction.DelayAction)
      ready_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, firstOrderId, InitiativeAction.ReadyAction)
    } else {
      next_btn.enabled = false
      delay_btn.enabled = false
      ready_btn.enabled = false
    }
    if (target.isDefined && target.get.isInOrder) {
      val comb = target.get
      executeReady_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, comb.orderId, InitiativeAction.ExecuteReady)
      updateMoveBeforeCombo(comb)
    } else {
      for (x <- this.contents if (x.isInstanceOf[Button])) {
        x.enabled = false
      }
    }
    updateLabels()
  }

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    _first = newState.orderFirst()
    combatState = newState
    //Validate context
    target = combatState.combatantOption(target.map(o => o.unifiedId))
    updatePanel()
  }

  val dockID = DockID("initiative")
  val dockTitle = "Initiative Actions"

  def dockFocusComponent: javax.swing.JComponent = {
    for (x <- contents) {
      if (x.peer.isInstanceOf[javax.swing.JButton] && x.enabled) return x.peer
    }
    null
  }

  def registerKeystroke() {
    KeystrokeBinder.bindKeystrokeAction(next_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt N", new ClickButtonAction("init.end", next_btn))
  }
}
