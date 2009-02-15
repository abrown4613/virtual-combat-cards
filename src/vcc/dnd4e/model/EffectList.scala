//$Id$
package vcc.dnd4e.model

case class EffectList(effects:List[Effect]) {
  def add(effect:Effect) = {
    EffectList(effect::effects)
  }
}
