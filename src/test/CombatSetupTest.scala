/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package test

import junit.framework._

import vcc.dnd4e.model._
import vcc.dnd4e.view._
import scala.actors.Actor._
import vcc.dnd4e.controller._

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
      vcc.dnd4e.view.actor.ClearSequence(),
      vcc.dnd4e.view.actor.Combatant(ViewCombatant(Symbol("K"),"Kantrex",44,5,null)),
      //vcc.view.actor.SetHealth('K,HealthTrackerSummary(44,0,HealthStatus.Ok,0)),
      vcc.dnd4e.view.actor.SetInitiative('K,InitiativeTracker(0,InitiativeState.Waiting))
    )),self)
    //val logmock=new ExpectActor(1000,new ExpectingBehavior(List(vcc.controller.actions.LogError("Nothing"))),self)
    val tracker=new vcc.controller.Tracker(null)
    tracker ! vcc.controller.actions.AddObserver(uimock)
    
    tracker.start
    
    tracker ! request.AddCombatant(tmpl)
    tracker ! request.Enumerate()
    
    receiveWithin(400) {
      case uimock.Done(s) if(s eq uimock)=> assert(true)
      case uimock.Timeout(s) if(s eq uimock) => assert(false,"Should termintate")
      case uimock.Feedback(s,w) if(s eq uimock) => assert(false,"Unexpeced feedback"+w)
    }
  }

}
