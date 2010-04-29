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
import vcc.util.swing._
import vcc.dnd4e.domain.tracker.common.Command.AddEffect
import vcc.dnd4e.domain.tracker.common._
import vcc.infra.docking._
import vcc.dnd4e.BootStrap
import vcc.dnd4e.model.common.CombatantType
import vcc.dnd4e.model.CombatantEntity
import vcc.dnd4e.domain.tracker.snapshot.{CombatState, StateChange}

class EffectEditorPanel(director: PanelDirector) extends MigPanel("fillx,hidemode 3")
        with CombatStateObserver with ContextObserver with ScalaDockableComponent
{
  private val memory = scala.collection.mutable.Map.empty[String, List[EffectEditor.StateMemento]]
  private var lastActiveKey: String = null
  private var _changing: Boolean = false

  case class ActiveCombatant(val c: CombatantRosterDefinition) {
    override def toString() = c.cid.id + " - " + c.entity.name
  }

  private var target: Option[Symbol] = None
  private var state = director.currentState

  val terrainCombatant = CombatantEntity(null, "Terrain or Other", MinionHealthDefinition(), 0, CombatantType.Minion, null)

  private val other = new ActiveCombatant(CombatantRosterDefinition(CombatantID("?"), null, terrainCombatant))
  val activeCombo = new ComboBox[ActiveCombatant](List(other))
  private val efpl = if (
    java.awt.Toolkit.getDefaultToolkit.getScreenSize().getHeight() > 700 &&
            BootStrap.getPropertyAsInt("vcc.view.efp.max", 3) > 2
  ) {
      List(new EffectEditor(this), new EffectEditor(this), new EffectEditor(this))
    } else {
    List(new EffectEditor(this), new EffectEditor(this))
  }

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
    //TODO Fix this we need a logic to get the IOI
    //      if (!_changing) director.setActiveCombatant(Some(activeCombo.selection.item.c.id))
    //      switchActive(activeCombo.selection.item.c.entity.name)
  }

  def combatStateChanged(newState: CombatState, uv: Array[UnifiedCombatant], changes: StateChange) {
    state = newState
    //TODO Check for simplification
    if (changes.changes.contains(StateChange.combat.Roster)) {
      //Sequence changed time to update ActiveCombo
      //TODO Move this the UV
      //      val seq = newState.roster.keys.toList
      //      if (seq.isEmpty) {
      //        activeCombo.peer.setModel(ComboBox.newConstantModel(List(other)))
      //        for (efp <- efpl) efp.setSequence(Nil)
      //      } else {
      //        var nac = seq.map(c => {new ActiveCombatant(c)}).toList ::: List(other)
      //        activeCombo.peer.setModel(ComboBox.newConstantModel(nac))
      //        val lid = seq.map(c => {c.id.name})
      //        for (efp <- efpl) efp.setSequence(lid)
      //      }
      //      switchActive(activeCombo.selection.item.c.entity.name)
    }
  }

  def changeContext(nctx: Option[CombatantID], isTarget: Boolean) {
    val ctx = state.getCombatant(nctx)
    //    if (isTarget) {
    //      for (efp <- efpl) efp.setContext(ctx)
    //      target = nctx
    //    } else if (ctx.isDefined) {
    //      //Setting Source information
    //      _changing = true
    //      activeCombo.selection.item = new ActiveCombatant(ctx.get)
    //      activeCombo.repaint
    //      _changing = false
    //    }
    //TODO This need to get the real deal and update subpanels
    0
  }

  def createEffect(subPanel: EffectSubPanelComboOption, durOption: DurationComboEntry, beneficial: Boolean) {
    //TODO Create new panels
    //    val source = activeCombo.selection.item.c
    //    val tgtComb = state.combatantMap(target.get)
    //    val cond = subPanel.generateEffect(source, tgtComb)
    //    val duration = durOption.generate(source, tgtComb)
    //    director.requestAction(AddEffect(target.get, Effect(source.id, cond, beneficial, duration)))
  }

  /**
   * Store data on the effect memory
   */
  def switchActive(nkey: String) {
    // If we have a key store it
    if (lastActiveKey != null) {
      memory(lastActiveKey) = efpl.map(epl => epl.saveMemento())
    }
    lastActiveKey = nkey
    // Restore the previous mementos if they exit
    if (memory.contains(nkey)) {
      efpl.zip(memory(nkey)).map(x => x._1.restoreMemento(x._2))
    }
  }

  val dockID = DockID("effect-editor")
  val dockTitle = "Effect Creation"
  val dockFocusComponent = null
}
