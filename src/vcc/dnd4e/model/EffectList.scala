//$Id$
package vcc.dnd4e.model

case class EffectList(effects:List[Effect]) {
  def add(effect:Effect) = {
    EffectList(effect::effects)
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
  
}
