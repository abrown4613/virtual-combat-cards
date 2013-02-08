/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
import vcc.util.swing._
import vcc.dnd4e.tracker.common._
import vcc.infra.docking._
import vcc.dnd4e.tracker.common.CombatantEntity
import vcc.dnd4e.tracker.common.Command.AddEffect
import scala.Some
import vcc.dnd4e.tracker.common.EffectList
import vcc.infra.docking.DockID
import vcc.dnd4e.tracker.common.CombatantRosterDefinition

class EffectEditorPanel(director: PanelDirector, numberOfEffectPanel: Int) extends MigPanel("fillx,hidemode 3")
with CombatStateObserver with ContextObserver with ScalaDockableComponent {

  private val memory = scala.collection.mutable.Map.empty[String, List[EffectEditor.StateMemento]]
  private var lastActiveKey: String = null
  private var _changing: Boolean = false

  private var target: Option[UnifiedCombatantID] = None
  private var state:UnifiedSequenceTable = null

  private object TerrainCombatantStateView extends CombatantStateView {
    private val terrainDefinition = CombatantEntity("vcc-ent:terrain", "Terrain or Other", MinionHealthDefinition, 0, null)

    def definition: CombatantRosterDefinition = CombatantRosterDefinition(otherId, null, terrainDefinition)

    def healthTracker: HealthTracker = null

    def effects: EffectList = null

    def comment: String = null
  }

  private val otherId = CombatantID.makeSpecialID("?")
  private val otherCombatant = new UnifiedCombatant(otherId, null, TerrainCombatantStateView)

  val activeModel = new ContainerComboBoxModel[UnifiedCombatant](List(otherCombatant))
  val activeCombo = new ExplicitModelComboBox[UnifiedCombatant](activeModel)
  activeCombo.setFormatRenderer(new StringFormatListCellRenderer[UnifiedCombatant](cmb => {
    (if (cmb.isInOrder) cmb.orderId.toLabelString else cmb.combId.id) + " - " + cmb.name
  }))

  private val effectEditorPanels = (1 to numberOfEffectPanel).map(x => new EffectEditor(this)).toList

  private var active: Option[UnifiedCombatantID] = Some(otherCombatant.unifiedId)

  add(new Label("Source:"), "span,split 2")
  add(activeCombo, "wrap,growx")
  for (efp <- effectEditorPanels) {
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
          for (efp <- effectEditorPanels) efp.setContext(Some(activeCombo.selection.item), false)
        }
      }
      switchActive(activeCombo.selection.item.definition.entity.eid)
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState

    activeModel.contents = state.elements ++ Seq(otherCombatant)
    setActiveComboSelection(active)
    for (efp <- effectEditorPanels) efp.setSequence(state.elements.map(c => c.combId))
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
    val idx = activeModel.entries.indexWhere(c => c.matches(cmb))
    activeCombo.selection.index = idx
    activeCombo.repaint()
    switchActive(cmb.definition.entity.eid)
  }

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    for (efp <- effectEditorPanels) efp.setContext(state.combatantOption(newContext), true)
    target = newContext
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    for (efp <- effectEditorPanels) efp.setContext(state.combatantOption(newContext), false)
    _changing = true
    active = newContext
    setActiveComboSelection(active)
    _changing = false
  }

  /**
   * Create the effect on the target
   */
  def createEffect(tgt: UnifiedCombatant, condition: Condition, duration: Duration, beneficial: Boolean) {
    val source = activeCombo.selection.item
    director.requestAction(AddEffect(tgt.combId, source.combId, condition, duration))
  }

  def getTargetCombatant: UnifiedCombatant = {
    state(target.get.combId, target.get.orderId)
  }

  /**
   * Store data on the effect memory
   */
  def switchActive(nkey: String) {
    storeEffectsAndLastActive()
    restorePreviousExistentMementos()

    def storeEffectsAndLastActive() {
      if (lastActiveKey != null)
        memory(lastActiveKey) = effectEditorPanels.map(epl => epl.saveMemento())
      lastActiveKey = nkey
    }

    def restorePreviousExistentMementos() {
      if (memory.contains(nkey))
        effectEditorPanels.zip(memory(nkey)).map(x => x._1.restoreMemento(x._2))
      else
        effectEditorPanels.foreach(ep => ep.clearPanel())
    }
  }

  val dockID = DockID("effect-editor")
  val dockTitle = "Effect Creation"
  val dockFocusComponent = null
}