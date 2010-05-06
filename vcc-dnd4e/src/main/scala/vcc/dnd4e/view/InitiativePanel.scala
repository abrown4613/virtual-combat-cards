/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import scala.swing._
import scala.swing.event._
import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.domain.tracker.common.InitiativeTracker.state._
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.infra.docking._
import vcc.dnd4e.domain.tracker.snapshot.{StateChange, CombatState}
import vcc.util.swing._

class InitiativePanel(director: PanelDirector) extends MigPanel("flowx,ins 2,hidemode 3", "[20%,fill][40%,fill][40%,fill]", "")
        with CombatStateObserver with ContextObserver with ScalaDockableComponent with KeystrokeContainer
{
  private val startRound_btn = new Button("Start turn")
  startRound_btn.tooltip = ("Start round of the first combatant")

  private val endRound_btn = new Button("End turn")
  endRound_btn.tooltip = ("End round of the first combatant")

  private val moveUp_btn = new Button("Move up & Start turn")
  private val delay_btn = new Button("Delay")
  delay_btn.tooltip = "Delay the round of the first combatant"
  private val ready_btn = new Button("Ready action")
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
  before_Combo.setFormatRenderer(new StringFormatListCellRenderer(o => o.toLabelString))

  moveBefore_btn.tooltip = "Move select combatant to a position before the combatant selected on the combo box to the left"

  private var context: Option[UnifiedCombatant] = None
  private var _first: UnifiedCombatant = null
  private var combatState = director.currentState

  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT;
  add(firstLabel, "align right")
  add(startRound_btn, "")
  add(endRound_btn, "")
  add(delay_btn, "wrap")
  add(ready_btn, "wrap")
  add(targetLabel, "align right")
  add(executeReady_btn)
  add(moveUp_btn, "wrap")
  add(moveLabel, "align right,span 2")
  add(before_Combo, "split 2")
  add(moveBefore_btn)

  for (x <- contents) listenTo(x)

  updatePanel()

  reactions += {
    case ButtonClicked(this.startRound_btn) if (_first != null) =>
      director requestAction InitiativeAction(_first.orderId, InitiativeTracker.action.StartRound)
    case ButtonClicked(this.endRound_btn) if (_first != null) =>
      director requestAction InitiativeAction(_first.orderId, InitiativeTracker.action.EndRound)
    case ButtonClicked(this.ready_btn) if (_first != null) =>
      director requestAction InitiativeAction(_first.orderId, InitiativeTracker.action.Ready)
    case ButtonClicked(this.delay_btn) if (_first != null) =>
      director requestAction InitiativeAction(_first.orderId, InitiativeTracker.action.Delay)

    case ButtonClicked(this.moveUp_btn) =>
      director requestAction InitiativeAction(context.get.orderId, InitiativeTracker.action.MoveUp)
    case ButtonClicked(this.executeReady_btn) =>
      director requestAction InitiativeAction(context.get.orderId, InitiativeTracker.action.ExecuteReady)

    case ButtonClicked(this.moveBefore_btn) if (before_Combo.selection.item != null) =>
      director requestAction MoveBefore(context.get.orderId, before_Combo.selection.item)
  }

  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) = {
    if (isTarget) {
      context = combatState.combatantOption(nctx)
    }
    if (nctx.isDefined && isTarget) updatePanel()
  }

  val rules = new CombatStateRules()

  private def updatePanel() {
    if (_first != null) {
      startRound_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, _first.orderId, InitiativeTracker.action.StartRound)
      endRound_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, _first.orderId, InitiativeTracker.action.EndRound)
      delay_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, _first.orderId, InitiativeTracker.action.StartRound)
      ready_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, _first.orderId, InitiativeTracker.action.Ready)
    } else {
      startRound_btn.enabled = false
      endRound_btn.enabled = false
      delay_btn.enabled = false
      ready_btn.enabled = false
    }
    if (context.isDefined && context.get.isInOrder) {
      val comb = context.get
      before_Combo.enabled = true
      moveLabel.enabled = true

      moveUp_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, comb.orderId, InitiativeTracker.action.MoveUp)
      executeReady_btn.enabled = rules.canInitiativeOrderPerform(combatState.state, comb.orderId, InitiativeTracker.action.ExecuteReady)

      //Get possible combatant to move before
      val before: Seq[InitiativeOrderID] = if (comb.isInOrder)
        combatState.elements.filter(c => rules.canMoveBefore(combatState.state, comb.orderId, c.orderId)).map {c => c.orderId}
      else
        Seq()
      candidateBefore.contents = before
      moveBefore_btn.enabled = !before.isEmpty

      toggleFirstButton(endRound_btn.enabled)
    } else {
      for (x <- this.contents if (x.isInstanceOf[Button])) {
        x.enabled = false
      }
      toggleFirstButton(false)
    }

    val selectedId: String = if (context.isDefined && context.get.isInOrder) context.get.orderId.toLabelString else null

    firstLabel.text = if (_first != null && _first.isInOrder) "[" + _first.orderId.toLabelString + "] can:" else "First can:"
    targetLabel.text = if (selectedId != null) "[" + selectedId + "] can:" else "Target can:"
    moveLabel.text = if (selectedId != null) "Move [ " + selectedId + " ] before:" else "Move target before:"
  }

  private def toggleFirstButton(canEnd: Boolean) {
    endRound_btn.visible = canEnd
    ready_btn.visible = canEnd
    startRound_btn.visible = !canEnd
    delay_btn.visible = !canEnd
  }

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    _first = if (newState.orderFirst.isDefined) newState.orderFirst.get else null
    combatState = newState
    //Validate context
    context = combatState.combatantOption(context.map(o => o.unifiedId))
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
    KeystrokeBinder.bindKeystrokeAction(startRound_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt S", new ClickButtonAction("init.start", startRound_btn))
    KeystrokeBinder.bindKeystrokeAction(endRound_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt E", new ClickButtonAction("init.end", endRound_btn))
  }
}
