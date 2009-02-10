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
}
case class InitiativeTracker(round:Int,state:vcc.dnd4e.model.InitiativeState.Value) {
  import InitiativeTracker.actions
  //Transform partial function parameters(first?,Action) 
  val transform:PartialFunction[(Boolean,actions.Value),InitiativeTracker] = {
    case (true,actions.StartRound) if(this.state==Waiting||this.state==Ready) => InitiativeTracker(round+1,Acting)
    case (true,actions.EndRound) if(state==Acting) =>  InitiativeTracker(round,Waiting)
    case (true,actions.EndRound) if(state==Delaying) => InitiativeTracker(round,Waiting)
    case (false,actions.MoveUp) if(state==Delaying)=> InitiativeTracker(round,Acting)
    case (first,actions.MoveUp) if(state==Reserve) => InitiativeTracker(round+1,Acting)
    case (true,actions.Ready) if(state==Acting) => InitiativeTracker(round,Ready)
    case (false,actions.ExecuteReady) if(state==Ready) => InitiativeTracker(round,Waiting)
    case (true,actions.Delay) if(state==Waiting||this.state==Ready) => InitiativeTracker(round+1,Delaying)
  }
}
