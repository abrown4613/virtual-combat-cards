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
import vcc.util.swing.{MigPanel, KeystrokeContainer, KeystrokeBinder, ClickButtonAction, ExplicitModelComboBox, ContainterComboBoxModel}
import vcc.infra.docking._
import vcc.dnd4e.domain.tracker.snapshot.{StateChange, CombatState}

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
  private val candidateBefore = new ContainterComboBoxModel[CombatantID](Nil)
  private val before_Combo = new ExplicitModelComboBox[CombatantID](candidateBefore)

  moveBefore_btn.tooltip = "Move select combatant to a position before the combatant selected on the combo box to the left"

  private var context: Option[CombatantID] = None
  private var _first: CombatantID = null
  private var combatState: CombatState = director.currentState

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

  @deprecated
  //TODO Nuke this
  def cid2ioi(id: CombatantID) = InitiativeOrderID(id, 0)

  updatePanel()

  reactions += {
    case ButtonClicked(this.startRound_btn) if (_first != null) =>
      director requestAction InitiativeAction(cid2ioi(_first), InitiativeTracker.action.StartRound)
    case ButtonClicked(this.endRound_btn) if (_first != null) =>
      director requestAction InitiativeAction(cid2ioi(_first), InitiativeTracker.action.EndRound)
    case ButtonClicked(this.ready_btn) if (_first != null) =>
      director requestAction InitiativeAction(cid2ioi(_first), InitiativeTracker.action.Ready)
    case ButtonClicked(this.delay_btn) if (_first != null) =>
      director requestAction InitiativeAction(cid2ioi(_first), InitiativeTracker.action.Delay)

    case ButtonClicked(this.moveUp_btn) =>
      director requestAction InitiativeAction(cid2ioi(context.get), InitiativeTracker.action.MoveUp)
    case ButtonClicked(this.executeReady_btn) =>
      director requestAction InitiativeAction(cid2ioi(context.get), InitiativeTracker.action.ExecuteReady)

    case ButtonClicked(this.moveBefore_btn) if (before_Combo.selection.item != null) =>
      director requestAction MoveBefore(cid2ioi(context.get), cid2ioi(before_Combo.selection.item))
  }

  def changeContext(nctx: Option[CombatantID], isTarget: Boolean) = {
    if (isTarget) {
      context = nctx
    }
    if (nctx.isDefined && isTarget) updatePanel()
  }

  private def updatePanel() {
    /*
        val cmbo = combatState.getCombatant(context)
        if (cmbo.isDefined) {
          val comb = cmbo.get
          val itt = comb.init
          val first = (context.get == _first.id)
          val state = itt.state
          if (_first != null) {
            startRound_btn.enabled = _first.init.canTransform(true, InitiativeTracker.actions.StartRound)
            endRound_btn.enabled = _first.init.canTransform(true, InitiativeTracker.actions.EndRound)
            delay_btn.enabled = _first.init.canTransform(true, InitiativeTracker.actions.StartRound)
            ready_btn.enabled = _first.init.canTransform(true, InitiativeTracker.actions.Ready)
          } else {
            startRound_btn.enabled = false
            endRound_btn.enabled = false
            delay_btn.enabled = false
            ready_btn.enabled = false
          }
          //TODO Use rules for this
          moveBefore_btn.enabled = state != Acting && state != InitiativeState.Reserve
          before_Combo.enabled = moveBefore_btn.enabled
          moveLabel.enabled = moveBefore_btn.enabled

          moveUp_btn.enabled = (
                  itt.canTransform(first, InitiativeTracker.actions.MoveUp) && (_first != null && (
                          ((state == InitiativeState.Delaying) && (_first.init.state != InitiativeState.Acting)) ||
                                  ((state == InitiativeState.Ready) && (_first.init.state == InitiativeState.Acting)) ||
                                  (state == InitiativeState.Reserve && (_first.init.state != InitiativeState.Acting))
                          )));
          executeReady_btn.enabled = (itt.canTransform(first, InitiativeTracker.actions.ExecuteReady) && _first.init.state == InitiativeState.Acting)

          // Get possible combatant to move before, exclude acting and reserver combatants
          val before = combatState.combatantSequence.filter {c => c.init.state != InitiativeState.Acting && c.init.state != InitiativeState.Reserve}.map {c => c.id.name}
          candidateBefore.contents = before
          before_Combo.selection.index = -1

          toggleFirstButton(endRound_btn.enabled)
        } else {
          for (x <- this.contents if (x.isInstanceOf[Button])) {
            x.enabled = false
          }
          toggleFirstButton(false)
        }
        firstLabel.text = if (_first != null) "[" + _first.id.name + "] can:" else "First can:"
        targetLabel.text = if (context.isDefined) "[" + context.get.name + "] can:" else "Target can:"
        moveLabel.text = if (context.isDefined) "Move [ " + context.get.name + " ] before:" else "Move target before:"
    */
  }

  private def toggleFirstButton(canEnd: Boolean) {
    endRound_btn.visible = canEnd
    ready_btn.visible = canEnd
    startRound_btn.visible = !canEnd
    delay_btn.visible = !canEnd
  }

  def combatStateChanged(newState: CombatState, uv: Array[UnifiedCombatant], changes: StateChange) {
    //TODO move this to UnifiedView
    _first = if (newState.order.isEmpty) null else newState.order(0).combId
    combatState = newState
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
