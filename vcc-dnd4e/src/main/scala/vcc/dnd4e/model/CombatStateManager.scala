//$Id$

/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

package vcc.dnd4e.model

import common._
import scala.actors.Actor
import scala.actors.Actor.{loop, react}

import vcc.controller.transaction.ChangeNotification

/**
 * This object is responsible for collecting and handling change notifications from the 
 * Tracker actor. I will collect them into a local representation and publish the immutable
 * CombatState to all registered listener.
 */
class CombatStateManager(tracker: Actor) {
  private val logger = org.slf4j.LoggerFactory.getLogger("domain")

  class Combatant(val id: Symbol, val alias: String, val entity: CombatantEntity) {
    var health = HealthTracker.createTracker(entity.healthDef)
    var initTracker = InitiativeTracker(0, InitiativeState.Reserve)
    var info: String = ""
    var effects: List[Effect] = Nil

    def toCombatantState: CombatantState = CombatantState(id, alias, entity, health, initTracker, info, effects)
  }

  private def fromCombatantState(cmb: CombatantState): Combatant = {
    val icmb = new Combatant(cmb.id, cmb.alias, cmb.entity)
    icmb.health = cmb.health
    icmb.initTracker = cmb.init
    icmb.info = cmb.info
    icmb.effects = cmb.effects
    icmb
  }

  class SupportActor extends Actor {
    type T = Combatant

    //Data structures to hold the objects
    private var _seq: Seq[Symbol] = Nil
    private val _map = scala.collection.mutable.Map.empty[Symbol, T]
    private var _cstate = CombatState(Map.empty[Symbol, CombatantState], Nil)
    private var _observers: List[CombatStateObserver] = Nil

    object InMap {
      def unapply(id: Symbol): Option[T] = if (id != null && _map.contains(id)) Some(_map(id)) else None
    }

    def act() {
      logger.debug("START ")
      tracker ! vcc.controller.message.AddObserver(this)
      loop {
        react {
          case CombatStateChanged(changes) =>
            val sc = processChanges(changes)
            publishCombatState(sc)
          case 'STATE =>
            reply(_cstate)
          case ('ADDOBS, obs: CombatStateObserver) =>
            _observers = obs :: _observers
          case s =>
            logger.error("CombatStateManager: Unhandled message {}", s)
        }
      }
    }

    private def processChanges(changes: Seq[CombatStateChange]): CombatStateChanges = {
      val sc = new CombatStateChanges()
      changes.foreach {
        _ match {
          case RosterUpdate(nmap) =>
            logger.debug("CombatStateManager: roster now contain: {}", nmap.keys.toList.toString)
            _map.clear
            nmap.foreach {me => _map += (me._1 -> fromCombatantState(me._2))}
          case SequenceChange(seq) =>
            _seq = seq
            sc.add(CombatState.part.Sequence)
          case CombatantUpdate(id, obj) if (_map.isDefinedAt(id)) =>
            obj match {
              case CombatantComment(text) =>
                _map(id).info = text
                sc.add(id, CombatantState.part.Note)
              case EffectList(el) =>
                _map(id).effects = el
                sc.add(id, CombatantState.part.Effects)
              case i: InitiativeTracker =>
                _map(id).initTracker = i
                sc.add(id, CombatantState.part.Initiative)
              case h: HealthTracker =>
                _map(id).health = h
                sc.add(id, CombatantState.part.Health)
            }
          case c: CombatantUpdate =>
            logger.error("CombatStateManger: Should not get here: {}", c)
        }
      }
      sc
    }

    private def publishCombatState(change: CombatStateChanges) {
      logger.debug("CombatStateManger: Current map: {}", _map)
      logger.debug("CombatStateManager: Changes: {}", change)
      val cmap: Map[Symbol, CombatantState] = Map(_map.map(me => (me._1, me._2.toCombatantState)).toSeq: _*)
      val newState = CombatState(cmap, _seq.map(id => cmap(id)))
      _cstate = newState
      _observers.foreach(obs => obs.combatStateChanged(newState, change))
    }
  }

  private val _actor = new SupportActor()
  _actor.start()

  /**
   * Get the current state. This is a actor resquest and may take some time to 
   * be handled. 
   */
  def currentState: CombatState = {
    _actor ! 'STATE
    Actor.self.receive {
      case s: CombatState => s
    }
  }

  def registerObserver(obs: CombatStateObserver) {
    _actor ! ('ADDOBS, obs)
  }
}
