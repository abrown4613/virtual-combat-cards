package test

import junit.framework._

import vcc.model._
import vcc.view._
import scala.actors.Actor._

//FIXME: This test is bad, and is not working.

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
      vcc.view.actor.ClearSequence(),
      vcc.view.actor.Combatant(ViewCombatant(Symbol("K"),"Kantrex",44,5)),
      vcc.view.actor.SetHealth('K,HealthTrackerSummary(44,0,HealthStatus.Ok,0)),
      vcc.view.actor.SetInitiative('K,InitiativeTracker(0,InitiativeState.Waiting))
    )),self)
    //val logmock=new ExpectActor(1000,new ExpectingBehavior(List(vcc.controller.actions.LogError("Nothing"))),self)
    val tracker=new vcc.controller.Tracker()
    tracker ! vcc.controller.actions.AddObserver(uimock)
    
    tracker.start
    
    tracker ! vcc.controller.actions.AddCombatant('K,tmpl)
    tracker ! vcc.controller.actions.Enumerate()
    
    receiveWithin(400) {
      case uimock.Done(s) if(s eq uimock)=> assert(true)
      case uimock.Timeout(s) if(s eq uimock) => assert(false,"Should termintate")
      case uimock.Feedback(s,w) if(s eq uimock) => assert(false,"Unexpeced feedback"+w)
    }
  }

}
