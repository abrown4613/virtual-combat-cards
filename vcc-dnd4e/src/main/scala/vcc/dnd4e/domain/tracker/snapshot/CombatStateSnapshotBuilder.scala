/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.controller.SnapshotBuilder
import vcc.controller.transaction.ChangeNotification
import vcc.dnd4e.domain.tracker.common._

/**
 * This class is used to collect CombatStateChanges and keep a version of the transactional tracker state.
 * It will generate snapshot.CombatState as snapshot.
 */
class CombatStateSnapshotBuilder extends SnapshotBuilder[CombatState] {

  /**
   * Internal representation of the combatant.
   */
  private class Combatant {
    var _effects: EffectList = null
    var _comment: String = null
    var _health: HealthTracker = null
    var _definition: CombatantRosterDefinition = null

    def setAspect(aspect: CombatantAspect) {
      aspect match {
        case h: HealthTracker => _health = h
        case CombatantComment(comment) => _comment = comment
        case el: EffectList => _effects = el
        case d: CombatantRosterDefinition => _definition = d
      }
    }

    def toView(): CombatantStateView = CombatantState(_definition, _health, _effects, _comment)
  }

  private var order: List[InitiativeOrderID] = Nil
  private val initiatives = scala.collection.mutable.Map.empty[InitiativeOrderID, InitiativeTracker]
  private var roster = Map.empty[CombatantID, Combatant]
  private var first: Option[InitiativeOrderID] = None
  private var comment = ""
  private var inCombat = false

  private var toBeFirst: Option[InitiativeOrderID] = None

  def processChange(change: ChangeNotification) {
    if (!change.isInstanceOf[CombatStateChange]) throw new Exception("Can't handle a: " + change)
    change.asInstanceOf[CombatStateChange] match {
      case InitiativeTrackerChange(initTracker) =>
        initiatives += (initTracker.orderID -> initTracker)

      case InitiativeOrderChange(newOrder) =>
        order = newOrder.map(x => x.orderID)
        initiatives ++= newOrder.map(it => it.orderID -> it)

      case RosterChange(newRoster) =>
        val nr = for ((key, aspects) <- newRoster) yield {
          val comb = new Combatant()
          aspects.map(a => comb.setAspect(a))
          (key -> comb)
        }
        roster = Map(nr.toSeq: _*)
      case CombatantChange(cid, aspect) =>
        if (!roster.isDefinedAt(cid)) throw new NoSuchElementException("No such combatant " + cid)
        roster(cid).setAspect(aspect)

      case CombatMetaDataChange(newInCombat, newComment) =>
        inCombat = newInCombat
        comment = newComment

      case InitiativeOrderFirstChange(who) =>
        toBeFirst = Some(who)

    }
  }

  def endChanges() {
    initiatives --= (initiatives.keys.toList -- order)
    if (toBeFirst.isDefined) {
      val who = toBeFirst.get
      if (who != null && !initiatives.isDefinedAt(who)) throw new NoSuchElementException("InitiativeOrder does not include: " + who)
      if (who == null) first = None
      else first = Some(who)
    }
  }

  def beginChanges() {}

  def getSnapshot(): CombatState = new CombatState(
    inCombat, comment,
    order, Map(initiatives.toSeq: _*), first,
    Map(roster.map(x => x._1 -> x._2.toView).toSeq: _*))
}