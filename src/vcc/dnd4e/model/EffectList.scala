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

}
