package vcc.model

import vcc.model.InitiativeState._

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
case class InitiativeTracker(round:Int,state:vcc.model.InitiativeState.Value) {
  import InitiativeTracker.actions
  //Transform partial function parameters(first?,Action) 
  val transform:PartialFunction[(Boolean,actions.Value),InitiativeTracker] = {
    case (true,actions.StartRound) if(this.state==Waiting||this.state==Ready) => InitiativeTracker(round+1,Acting)
    case (true,actions.EndRound) if(state==Acting) =>  InitiativeTracker(round,Waiting)
    case (true,actions.EndRound) if(state==Delaying) => InitiativeTracker(round+1,Waiting)
    case (false,actions.MoveUp) if(state==Delaying)=> InitiativeTracker(round+1,Acting)
    case (first,actions.MoveUp) if(state==Reserve) => InitiativeTracker(round+1,Acting)
    case (true,actions.Ready) if(state==Acting) => InitiativeTracker(round,Ready)
    case (false,actions.ExecuteReady) if(state==Ready) => InitiativeTracker(round,Waiting)
    case (true,actions.Delay) if(state==Waiting) => InitiativeTracker(round,Delaying)
  }
}
