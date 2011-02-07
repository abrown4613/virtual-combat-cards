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

import swing._
import swing.event._
import vcc.util.swing.{MigPanel, ClickButtonAction, KeystrokeContainer}
import vcc.util.swing.KeystrokeBinder
import vcc.infra.docking._

import vcc.dnd4e.domain.tracker.common.Command._

class DamageCommandPanel(val director: PanelDirector)
  extends MigPanel("ins 2", "[fill][fill][fill][fill]", "") with KeystrokeContainer
  with ContextObserver with ScalaDockableComponent with SimpleCombatStateObserver {
  val dockID = DockID("damage")

  val dockTitle = "Health Change"

  private val damage = new TextField {
    columns = 3
    enabled = false
  }

  val dockFocusComponent = damage.peer

  private var target: Option[UnifiedCombatantID] = None

  private val badColor = new java.awt.Color(255, 228, 196)
  damage.background = badColor

  private val damage_btn = new Button("Damage")
  damage_btn.tooltip = "Apply damage to selected combatant (shortcut Alt-D)"

  private val heal_btn = new Button("Heal")
  heal_btn.tooltip = "Heal selected combatant (shortcut Alt-H)"

  private val temp_btn = new Button("Set Temporary")
  temp_btn.tooltip = "Set Temporary hitpoints on selected combatant; will keep highest value (shortcut Alt-T)"

  private val death_btn = new Button("Fail Death Save")
  private val undie_btn = new Button("\"undie\"")
  undie_btn.tooltip = "Use this button to bring a dead combatant back to dying state. This will clear death strikes."
  private val controls = List(damage, damage_btn, heal_btn, temp_btn, death_btn, undie_btn)
  private val damageRelButton = List(damage_btn, heal_btn, temp_btn)

  private var damageEquation: helper.DamageParser.Term = null

  add(new Label("Hit Points:"))
  add(damage, "wrap")
  add(damage_btn, "skip 1")
  add(heal_btn)
  add(temp_btn, "wrap")
  add(undie_btn, "skip 1,align left")
  add(death_btn, "align left,span 2")
  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT;
  for (x <- controls) listenTo(x)
  listenTo(damage)
  changeContext(None, true)

  reactions += {
    case ValueChanged(this.damage) =>
      damageEquation = try {
        helper.DamageParser.parseString(damage.text)
      } catch {
        case _ => null
      }
      enableDamageControls(damageEquation != null)
    case FocusGained(this.damage, other, temporary) =>
      director.setStatusBarMessage("Enter equation with: + - * / and parenthesis and variable: 's' for surge value ; 'b' for bloody value")
      damage.selectAll()
    case FocusLost(this.damage, other, temp) =>
      director.setStatusBarMessage("")

    case ButtonClicked(this.death_btn) =>
      director requestAction FailDeathSave(target.get.combId)

    case ButtonClicked(this.undie_btn) =>
      director requestAction RevertDeath(target.get.combId)

    case ButtonClicked(button) if (damageEquation != null) => {
      val tgt = combatState.combatantOption(target).get
      val cinfo = Map(
        "b" -> tgt.health.base.totalHP / 2,
        "s" -> tgt.health.base.totalHP / 4
      )
      val value = damageEquation.apply(cinfo)
      if (value >= 0)
        button match {
          case this.damage_btn => director requestAction ApplyDamage(target.get.combId, value)
          case this.heal_btn => director requestAction HealDamage(target.get.combId, value)
          case this.temp_btn => director requestAction SetTemporaryHP(target.get.combId, value)
        }
    }
  }

  def enableDamageControls(enable: Boolean) {
    damage.background = if (enable) java.awt.Color.white else badColor
    for (x <- damageRelButton) x.enabled = enable
  }

  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (isTarget) {
      target = nctx
      controls map (x => x.enabled = target != None)
    }
  }

  def getRootPane = this.peer.getRootPane

  def registerKeystroke() {
    KeystrokeBinder.bindKeystrokeAction(damage_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt D", new ClickButtonAction("health.damage", damage_btn))
    KeystrokeBinder.bindKeystrokeAction(heal_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt H", new ClickButtonAction("health.heal", heal_btn))
    KeystrokeBinder.bindKeystrokeAction(temp_btn, true, KeystrokeBinder.FocusCondition.WhenWindowFocused, "alt T", new ClickButtonAction("health.settemphp", temp_btn))
  }
}