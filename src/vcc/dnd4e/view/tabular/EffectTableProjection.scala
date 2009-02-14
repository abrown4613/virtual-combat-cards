//$Id$
package vcc.dnd4e.view.tabular

import vcc.util.swing.TableModelRowProjection
import vcc.dnd4e.model.Effect

object EffectTableProjection extends TableModelRowProjection[Effect]{
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Src",classOf[String]),
    ("End",classOf[String]),
    ("Description",classOf[String]))
  
  def apply(col:Int,entry:Effect):java.lang.Object = {
    col match {
      case 0 => entry.source
      case 1 => entry.duration
      case 2 => entry.desc
    }
  }
  
 val setter:PartialFunction[(Int,Effect,Any),Unit]=null
  
}
