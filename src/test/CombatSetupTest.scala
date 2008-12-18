package test

import junit.framework._

import vcc.model._
import vcc.view._
import scala.actors.Actor._



class CombatSetupTest extends junit.framework.TestCase {

  def testLoadOneCharacter() {
    var tmpl=CombatantTemplate.fromXML(<character id="k" name="Kantrex" hp="44" init="5"><defense ac="23" will="18" reflex="17" fortitude="18" /> </character>)
    // Just a sanity check
    assert(tmpl.name=="Kantrex")
    assert(tmpl.ctype==CombatantType.Character)
    assert(tmpl.init==5)
    assert(tmpl.hp==44)
    assert(tmpl.id=="K")

    val uimock=new ExpectActor(200,new ExpectingBehavior(List(
      vcc.view.actor.Combatant(ViewCombatant(Symbol("K"),"Kantrex",44,5)),
      vcc.view.actor.SetHealth('K,HealthTrackerSummary(44,0,HealthStatus.Ok,0)),
      vcc.view.actor.SetInitiative('K,InitiativeTracker(0,InitiativeState.Waiting))
    )),self)
    val logmock=new ExpectActor(1000,new ExpectingBehavior(List(actions.LogError("Nothing"))),self)
    val tracker=new vcc.model.Tracker(logmock)
    tracker.setUserInterfaceActor(uimock)
    
    tracker.start
    
    tracker ! actions.AddCombatant('K,tmpl)
    
    receiveWithin(400) {
      case uimock.Done(s) if(s eq uimock)=> assert(true)
      case uimock.Timeout(s) if(s eq uimock) => assert(false,"Should termintate")
      case uimock.Feedback(s,w) if(s eq uimock) => assert(false,"Unexpeced feedback"+w)
    }
    logmock ! actions.LogError("Nothing")
    receiveWithin(500) {
      case logmock.Done(s) if(s eq logmock)=> assert(true)
      case logmock.Timeout(s) if(s eq logmock) => assert(false,"Should termintate")
      case logmock.Feedback(s,w) if(s eq logmock) => assert(false,"Unexpeced feedback"+w)
    }
  }

}
