//$Id$
package vcc.dnd4e.view.tabular

import vcc.util.swing.TableModelRowProjection
import vcc.dnd4e.model.{Effect,Condition}

class EffectTableProjection(tracker:scala.actors.Actor) extends TableModelRowProjection[(Symbol,Int,Effect)]{
  
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Src",classOf[String]),
    ("End",classOf[String]),
    ("Description",classOf[String]))
  
  def apply(col:Int,entry:(Symbol,Int,Effect)):java.lang.Object = {
    col match {
      case 0 => entry._3.source.name
      case 1 => entry._3.duration.shortDesc
      case 2 => entry._3.condition.description
    }
  }
  
  val setter:PartialFunction[(Int,(Symbol,Int,Effect),Any),Unit]= {
    case (2,(who,pos,Effect(_,Condition.Generic(x),_,_)),newvalue) =>
      tracker ! vcc.dnd4e.controller.request.UpdateEffect(who,pos,Condition.Generic(newvalue.asInstanceOf[String]))
  }
}
