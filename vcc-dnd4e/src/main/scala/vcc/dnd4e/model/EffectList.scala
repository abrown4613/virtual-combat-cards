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
//$Id$
package vcc.dnd4e.model

/**
 * This power
 */
case class EffectList(effects:List[Effect]) extends CombatantAspect {
  
  def add(effect:Effect):EffectList = {
    def addMark(effect:Effect,effects:List[Effect]):List[Effect] = {
      effects match {
        case Nil => List(effect)
        case Effect(src,Condition.Mark(marker,perm),bene,duration) :: rest =>
          if(perm) effects
          else effect::rest
        case eff :: rest => eff::addMark(effect,rest)
      }
    }
    
    effect match {
      case Effect(src,Condition.Mark(marker,perm),bene,duration) =>
        EffectList(addMark(effect,effects))
      case Effect(src,cond,bene,Effect.Duration.Stance) =>
        EffectList(effect::(effects.filter(e=>e.duration!=Effect.Duration.Stance)))
      case _ => EffectList(effect::effects) 
    }
  }
  
  /**
   * Remove an effect from the list
   */
  def delete(pos:Int):EffectList = {
    if(pos >= 0 && pos < effects.length) {
      val l=effects.slice(0,pos) ++ effects.slice(pos+1,effects.length)
      if(l == effects) this
      else EffectList(l)
    } else 
      this
  }
  
  /**
   * Update an effect int the list. Will only update generic effect.
   * @param pos The position of the effect in the list
   * @param newcond The new condition
   * @return The new EffectList
   */
  def update(pos:Int,newcond:Condition):EffectList = {
    if(pos >= 0 && pos < effects.length && effects(pos).condition.isInstanceOf[Condition.Generic] ) {
      
      val l=effects.slice(0,pos) ++ (effects(pos).updateCondition(newcond):: effects.slice(pos+1,effects.length))
      if(l == effects) this
      else EffectList(l)
    } else 
      this
  }
  
  protected def applyAndFilter(f:Effect=>Effect):List[Effect] = {
    effects.map(f).filter(e=> e!=null)
  } 
  
  /**
   * Process start of round on the duration
   */
  def startRound(cid:Symbol) = {
    EffectList(applyAndFilter(e=>e.startRound(cid)))
  }

  /**
   * Process end of round on the duration
   */
  def endRound(cid:Symbol) = {
    EffectList(applyAndFilter(e=>e.endRound(cid)))
  }

  /**
   * Process end of encounter (which is really the rest after combat)
   */
  def applyRest() = {
    EffectList(applyAndFilter(e=>e.applyRest()))
  }

  /**
   * Sustain the effect on a given position
   */
  def sustain(pos:Int):EffectList = {
    val eff=effects(pos).sustain()
    val neffs=effects.slice(0,pos) ++ List(eff) ++ effects.slice(pos+1,effects.length)
    EffectList(neffs)
  }
  
  /**
   * Process delay to all effects in the list
   */
  def processDelay(ally:Boolean,who:Symbol):EffectList = {
    EffectList(applyAndFilter(e=>e.processDelay(ally,who)))
  }
}
