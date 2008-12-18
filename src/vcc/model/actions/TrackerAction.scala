//$Id$
package vcc.model.actions

class TrackerAction {

}

// Builing the combat grid
case class AddCombatant(id:Symbol,template:CombatantTemplate)
case class Enumerate(peer:scala.actors.Actor)
case class ClearCombatants(all:Boolean)
case class StartCombat(order:Seq[Symbol])
case class EndCombat()

case class SetComment(to:Symbol,text:String)

case class ApplyDamage(to:Symbol,damage:Int)
case class SetTemporaryHP(to:Symbol,temphp:Int)
case class HealDamage(to:Symbol,heal:Int)
case class FailDeathSave(to:Symbol)


case class StartRound(who:Symbol)
case class EndRound(who:Symbol)
case class Ready(who:Symbol)
case class Delay(who:Symbol)
case class MoveUp(who:Symbol)
case class ExecuteReady(who:Symbol)

case class LogError(msg:String)
