//$Id$
package vcc.dnd4e.model

import vcc.dnd4e.model.InitiativeState._

//TODO: If the first was is not acting, should not be able to execute readied actions. Maybe
// initiative transformation should depend on the first char in the sequence (cmb). 
// if cmb.it == this we are the first
//  otherwise it's some ones else's action                                                                                                                                                        

object InitiativeTracker {
  object actions extends Enumeration {
    val StartRound=Value("Start Round")
    val EndRound=Value("End Round")
    val MoveUp=Value("Move Up")
    val Delay=Value("Delay")
    val Ready=Value("Ready Action")
    val ExecuteReady=Value("Execute Ready")
  }  
  val transform:PartialFunction[(InitiativeTracker,Boolean,actions.Value),InitiativeTracker] = {
    case (it,true,actions.StartRound) if(it.state==Waiting||it.state==Ready) => InitiativeTracker(it.round+1,Acting)
    case (it,true,actions.EndRound) if(it.state==Acting) =>  InitiativeTracker(it.round,Waiting)
    case (it,true,actions.EndRound) if(it.state==Delaying) => InitiativeTracker(it.round,Waiting)
    case (it,false,actions.MoveUp) if(it.state==Delaying)=> InitiativeTracker(it.round,Acting)
    case (it,first,actions.MoveUp) if(it.state==Reserve) => InitiativeTracker(it.round,Waiting)
    case (it,true,actions.Ready) if(it.state==Acting) => InitiativeTracker(it.round,Ready)
    case (it,false,actions.ExecuteReady) if(it.state==Ready) => InitiativeTracker(it.round,Waiting)
    case (it,true,actions.Delay) if(it.state==Acting) => InitiativeTracker(it.round,Delaying)
  }
  val metaTransform:PartialFunction[(InitiativeTracker,Boolean,actions.Value),InitiativeTracker] = {
    case (it,true,actions.StartRound) if(it.state==Waiting||it.state==Ready) => InitiativeTracker(it.round+1,Acting)
    case (it,true,actions.EndRound) if(it.state==Acting) =>  InitiativeTracker(it.round,Waiting)
    case (it,true,actions.EndRound) if(it.state==Delaying) => InitiativeTracker(it.round,Waiting)
    case (it,false,actions.MoveUp) if(it.state==Delaying)=> InitiativeTracker(it.round,Acting)
    case (it,first,actions.MoveUp) if(it.state==Reserve) => InitiativeTracker(it.round+1,Acting)
    case (it,true,actions.Ready) if(it.state==Acting) => InitiativeTracker(it.round,Ready)
    case (it,false,actions.ExecuteReady) if(it.state==Ready) => InitiativeTracker(it.round,Waiting)
    case (it,true,actions.Delay) if(it.state==Waiting||it.state==Ready) => InitiativeTracker(it.round+1,Delaying)
  }

}
case class InitiativeTracker(round:Int,state:vcc.dnd4e.model.InitiativeState.Value) {
  import InitiativeTracker.actions

  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return True if the transformation is valid
   */
  def canTransform(first:Boolean,action:InitiativeTracker.actions.Value):Boolean = InitiativeTracker.transform.isDefinedAt(this,first,action)
  
  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return The new InitiativeTracker with changes applied
   */
  def transform(first:Boolean,action:InitiativeTracker.actions.Value):InitiativeTracker = InitiativeTracker.transform(this,first,action)
}
