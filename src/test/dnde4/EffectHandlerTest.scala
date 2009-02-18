//$Id$
package test.dnde4

import junit.framework.TestCase
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import vcc.controller.TrackerController
import vcc.controller.transaction._
import vcc.dnd4e.controller._ 

import test.helper.TrackerMockup

class EffectHandlerTest extends TestCase {
  
  var mockTracker:TrackerMockup[TrackerContext]=null
  
  override def setUp() {
    val context=new TrackerContext()
    val loadHandler=new TrackerContextHandler(context)
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    loadHandler((trans1,actions.AddCombatant(new CombatantTemplate("Figher",40,5,CombatantType.Character){id="A"})))
    loadHandler((trans1,actions.AddCombatant(new CombatantTemplate("Monster",80,5,CombatantType.Character))))
    
    trans1.commit(trans1pub)
    assert(trans1pub.set.contains(vcc.dnd4e.view.actor.SetSequence(List('A,Symbol("1")))))
    
    //Setup the test subjects
    mockTracker=new TrackerMockup(new TrackerController(context) {
      addHandler(new TrackerEffectHandler(context))
      addPublisher(new TrackerEffectPublisher(context))
    })
  }
  
  /**
   * Add a effect to the target, this will add two general effects and make sure they are there
   * and changes get published.
   */
  def testAddEffects() {
    // Just for sanity
    val src=Symbol("1")
    assert(mockTracker!=null)
    val trans1=new Transaction()
    
    //Load first effect 
    val ef1=Effect(src,Condition.Mark(src,false),false,Effect.Duration.EndOfEncounter)
    mockTracker.dispatch(request.AddEffect('A,ef1))
    assert(mockTracker.lastChangeMessages == List(response.UpdateEffects('A,List(ef1))),mockTracker.lastChangeMessages)

    val ef2=Effect(src,Condition.Generic("slowed"),false,Effect.Duration.EndOfEncounter)
    mockTracker.dispatch(request.AddEffect('A,ef2))
    assert(mockTracker.lastChangeMessages != Nil,mockTracker.lastChangeMessages)
    
    extractEffectList(mockTracker.lastChangeMessages.head) match {
      case Some(('A,el:List[_])) => 
        assert(el.contains(ef1))
        assert(el.contains(ef2))
      case None => assert(false,"Cant find notification in this message")
    }
  }
  
  def extractEffectList(m:Any):Option[(Symbol,List[Effect])] = {
    m match {
      case response.UpdateEffects(w,el) => Some((w,el))
      case _ => None
    }
  }
}