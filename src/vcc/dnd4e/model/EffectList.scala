//$Id$
package vcc.dnd4e.model

case class EffectList(effects:List[Effect]) {
  def add(effect:Effect) = {
    EffectList(effect::effects)
  }
  
  def delete(pos:Int):EffectList = {
    if(pos >= 0 && pos < effects.length) {
      val l=effects.slice(0,pos) ++ effects.slice(pos+1,effects.length)
      if(l == effects) this
      else EffectList(l)
    } else 
      this
  }
}
