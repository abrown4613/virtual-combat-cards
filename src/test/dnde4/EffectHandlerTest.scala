//$Id$
package test.dnde4

import junit.framework.TestCase
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import vcc.controller.TrackerController
import vcc.controller.transaction._
import vcc.dnd4e.controller._ 

import test.helper._

import vcc.dnd4e.model.Effect._

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
  
  def makeEffectsNameOnAExtractor() = {
    new test.helper.Matcher[List[String]]({
      case response.UpdateEffects('A,el) =>
          el.map(e=>e.condition.description+":"+e.duration.shortDesc)
    })
  }
  
  /**
   * Simple test to make sure that the list 'lst' has the element of 'must' (in any order)
   */
  def listMustContainOnly[T](lst:List[T],must:List[T]) {
    val chk1=lst -- must
    assert(chk1==Nil,"List contains more than expected:"+chk1)
    val chk2=must -- lst
    assert(chk2==Nil,"List does not contain:"+chk2)
  }

  /**
   * Make sure the the reponse matches matcher and has elements in lst
   */
  def responseMustHave[T](matcher:Matcher[List[T]],lst:List[T]) {
    mockTracker.lastChangeMessages match {
      case matcher.findAll(eff) => listMustContainOnly(eff,lst) 
      case _ => assert(false,"Last changes dont match 'matcher' "+mockTracker.lastChangeMessages)
    }
  }
  
  /**
   * Extract a list of exactly one match and return it, or fail assertion
   */
  def extractSingleEffectListOrFail[T](matcher:Matcher[List[T]]):List[T] = {
    mockTracker.lastChangeMessages match {
      case matcher.findAll(eff) => eff
      case _ => 
        assert(false,"Cant get effect list for matcher "+matcher)
        Nil
    }
  }
  
  def testCancelEffect() {
    val src=Symbol("1")
    loadEffect('A, Seq(
      Effect(src,Condition.Generic("ef1"),false,Duration.Other),
      Effect(src,Condition.Generic("ef2"),false,Duration.Other),
      Effect(src,Condition.Generic("ef3"),false,Duration.Other)
    ))
    
    val aeffs=makeEffectsNameOnAExtractor()
    val aeffs.findAll(eol) = mockTracker.lastChangeMessages
    assert(eol.length==3)
    
    // Should remove second element, and not change order.
    mockTracker.dispatch(request.CancelEffect('A,1))
    val elac1=extractSingleEffectListOrFail(aeffs)
    assert(elac1==List(eol.head,eol.last), "List does not match"+List(eol.head,eol.last))
    
    // Out of bounds, should not change
    mockTracker.dispatch(request.CancelEffect('A,2))
    assert(mockTracker.lastChangeMessages==Nil,mockTracker.lastChangeMessages)
    
    mockTracker.dispatch(request.CancelEffect('A,0))
    val elac0=extractSingleEffectListOrFail(aeffs)
    assert(elac0==List(eol.last),"List does not match"+List(eol.last))

    mockTracker.dispatch(request.CancelEffect('A,0))
    val elacl=extractSingleEffectListOrFail(aeffs)
    assert(elacl==Nil,"list should be empty")

    // Remove form empty list should not change
    mockTracker.dispatch(request.CancelEffect('A,2))
    assert(mockTracker.lastChangeMessages==Nil,mockTracker.lastChangeMessages)

  }
  
  
  @deprecated def extractEffectList(m:Any):Option[(Symbol,List[Effect])] = {
    m match {
      case response.UpdateEffects(w,el) => Some((w,el))
      case _ => None
    }
  }
  
  def loadEffect(to:Symbol,effects:Seq[Effect]) {
    val toelm= new test.helper.Matcher[List[Effect]]({ case response.UpdateEffects(to,el) => el })
    for(eff<-effects) {
      mockTracker.dispatch(request.AddEffect(to,eff))
      mockTracker.lastChangeMessages match {
        case toelm.findAll(el) if(el.contains(eff)) => 
        case _ => assert(false,"Effect not in effect list")
      }
    }
  }
}