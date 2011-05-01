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

import scala.swing._
import vcc.util.swing._
import vcc.dnd4e.domain.tracker.common.Command.AddEffect
import vcc.dnd4e.tracker.common._
import vcc.infra.docking._
import vcc.dnd4e.BootStrap
import vcc.dnd4e.domain.tracker.snapshot.{CombatantState, StateChange}
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.tracker.common.{CombatantEntity, CombatantType}

class EffectEditorPanel(director: PanelDirector) extends MigPanel("fillx,hidemode 3")
with CombatStateObserver with ContextObserver with ScalaDockableComponent {
  private val memory = scala.collection.mutable.Map.empty[EntityID, List[EffectEditor.StateMemento]]
  private var lastActiveKey: EntityID = null
  private var _changing: Boolean = false

  private var target: Option[UnifiedCombatantID] = None
  private var state = director.currentState

  val terrainDefinition = CombatantEntity(EntityID.fromName("terrain"), "Terrain or Other", MinionHealthDefinition, 0, CombatantType.Minion, null)

  private val otherId = CombatantID("?")
  private val otherCombatant = new UnifiedCombatant(otherId, null, CombatantState(CombatantRosterDefinition(otherId, null, terrainDefinition), null, null, null))
  val activeModel = new ContainerComboBoxModel[UnifiedCombatant](List(otherCombatant))
  val activeCombo = new ExplicitModelComboBox[UnifiedCombatant](activeModel)
  activeCombo.setFormatRenderer(new StringFormatListCellRenderer(cmb => {
    (if (cmb.isInOrder) cmb.orderId.toLabelString else cmb.combId.id) + " - " + cmb.name
  }))

  private val efpl = if (
    java.awt.Toolkit.getDefaultToolkit.getScreenSize().getHeight() > 700 &&
      BootStrap.getPropertyAsInt("vcc.view.efp.max", 3) > 2
  ) {
    List(new EffectEditor(this), new EffectEditor(this), new EffectEditor(this))
  } else {
    List(new EffectEditor(this), new EffectEditor(this))
  }

  private var active: Option[UnifiedCombatantID] = Some(otherCombatant.unifiedId)

  add(new Label("Source:"), "span,split 2")
  add(activeCombo, "wrap,growx")
  for (efp <- efpl) {
    addSeparator("Effect")
    add(efp, "span 2,wrap,grow x")
  }

  // Set and handle reaction to changing the active characters
  listenTo(activeCombo.selection)
  reactions += {
    case event.SelectionChanged(`activeCombo`) =>
      if (!_changing) {
        director.setActiveCombatant(Some(activeCombo.selection.item.unifiedId))
        if (activeCombo.selection.item.unifiedId.combId == otherId) {
          // This is used to reset target for Terrain
          for (efp <- efpl) efp.setContext(Some(activeCombo.selection.item), false)
        }
      }
      switchActive(activeCombo.selection.item.definition.entity.eid)
  }

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    state = newState
    if (StateChange.hasSequenceChange(changes.changes)) {
      //Sequence changed time to update ActiveCombo
      activeModel.contents = state.elements ++ Seq(otherCombatant)
      setActiveComboSelection(active)
      for (efp <- efpl) efp.setSequence(state.elements.map(c => c.combId))
    }
  }

  /**
   * This is used by internal editor to set the status message on the VCC status bar.
   */
  private[view] def setStatusBarMessage(s: String) {
    director.setStatusBarMessage(s)
  }

  private def setActiveComboSelection(who: Option[UnifiedCombatantID]) {
    val ctx = state.combatantOption(who)
    val cmb: UnifiedCombatant = if (ctx == None) otherCombatant else ctx.get
    val idx = activeModel.entries.findIndexOf(c => c.matches(cmb))
    activeCombo.selection.index = idx
    activeCombo.repaint
    switchActive(cmb.definition.entity.eid)
  }

  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (isTarget) {
      for (efp <- efpl) efp.setContext(state.combatantOption(nctx), true)
      target = nctx
    } else {
      for (efp <- efpl) efp.setContext(state.combatantOption(nctx), false)
      _changing = true
      active = nctx
      setActiveComboSelection(active)
      _changing = false
    }
  }

  def createEffect(subPanel: EffectSubPanelComboOption, durOption: DurationComboEntry, beneficial: Boolean) {
    val source = activeCombo.selection.item
    val tgtComb = state(target.get.combId, target.get.orderId)
    val cond = subPanel.generateEffect(source, tgtComb)
    val duration = durOption.generate(source, tgtComb)
    director.requestAction(AddEffect(target.get.combId, source.combId, cond, duration))
  }

  /**
   * Store data on the effect memory
   */
  def switchActive(nkey: EntityID) {
    // If we have a key store it
    if (lastActiveKey != null) {
      memory(lastActiveKey) = efpl.map(epl => epl.saveMemento())
    }
    lastActiveKey = nkey
    // Restore the previous mementos if they exit
    if (memory.contains(nkey)) {
      efpl.zip(memory(nkey)).map(x => x._1.restoreMemento(x._2))
    } else {
      efpl.foreach(ep => ep.clearPanel())
    }
  }

  val dockID = DockID("effect-editor")
  val dockTitle = "Effect Creation"
  val dockFocusComponent = null
}
