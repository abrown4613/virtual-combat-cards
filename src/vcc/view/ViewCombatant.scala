//$Id$
package vcc.view 

import vcc.model._

trait UIFormatted {
  def formattedText():String
}	

case class ViewCombatant(id:Symbol,name:String,hp:Int,init:Int,defense:DefenseBlock) {
  var health=HealthTrackerSummary(hp,0,HealthStatus.Ok,0)
  var initTracker=InitiativeTracker(0,InitiativeState.Reserve)
  var info:String=""
}

object ViewCombatantProjection extends vcc.util.swing.TableModelRowProjection[ViewCombatant] {
  override val columns=List[(String,java.lang.Class[_])](
    ("ID",classOf[java.lang.String]),
    ("Name",classOf[String]),
    ("Health",classOf[String]),
    ("Status",classOf[String]),
    ("Turn #",classOf[Integer]),
    ("Sequence",classOf[String])
  );
  def apply(col:Int,comb:ViewCombatant):java.lang.Object= {
    col match {
      case 0 => comb.id.name
      case 1 => comb.name
      case 2 => comb.health.currhp + " / "+comb.hp + (if(comb.health.temphp>0) " +"+comb.health.temphp else "")
      case 3 => comb.health.status + (if(comb.health.status==HealthStatus.Dying) ("(" + comb.health.deathstrikes + "/3)") else "") 
      case 4 => int2Integer(comb.initTracker.round)
      case 5 => comb.initTracker.state
    }
  }
  val setter = null
}
