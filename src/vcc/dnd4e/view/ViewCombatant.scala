//$Id$
package vcc.dnd4e.view 

import vcc.dnd4e.model._

trait UIFormatted {
  def formattedText():String
}	

case class ViewCombatant(id:Symbol,name:String,hp:Int,init:Int,defense:DefenseBlock) {
  var health=HealthTracker.createTracker(CharacterHealthDefinition(hp,hp/4,4))
  var initTracker=InitiativeTracker(0,InitiativeState.Reserve)
  var info:String=""
  var effects:List[Effect]=Nil
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
      case 2 => comb.health.currentHP + " / "+comb.hp + (if(comb.health.temporaryHP>0) " +"+comb.health.temporaryHP else "")
      case 3 => comb.health.status + (if(comb.health.status==HealthTracker.Status.Dying) ("(" + comb.health.deathStrikes + "/3)") else "!!!".substring(0,comb.health.deathStrikes)) 
      case 4 => int2Integer(comb.initTracker.round)
      case 5 => comb.initTracker.state
    }
  }
  val setter = null
}
