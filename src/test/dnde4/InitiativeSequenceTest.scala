//$Id$
package test.dnde4

import junit.framework.TestCase
import vcc.controller.TransactionalProcessor
import vcc.controller.TrackerController
import vcc.controller.transaction.Transaction
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import InitiativeState._
import vcc.dnd4e.controller._

//TODO: Must deprecate
import  vcc.dnd4e.view.actor._ 

import test.helper._

trait ActionRecorder {
  def actionsExecuted:List[TransactionalAction]
}

trait ActionAccumulator extends TransactionalProcessor[TrackerContext]{
  self: TransactionalProcessor[TrackerContext] =>
  
  var actionsProcessed:List[TransactionalAction]=Nil
  
  override def rewriteEnqueue(action:TransactionalAction) {
    actionsProcessed=Nil
    super.rewriteEnqueue(action)
  }
  
  addHandler {
    case msg =>
      actionsProcessed= actionsProcessed ::: List(msg)
  }
}


class InitiativeSequenceTest extends TestCase {
  var mockTracker:TrackerMockup[TrackerContext] with ActionRecorder =null
  val seqMatcher = new Matcher[Seq[Symbol]]({
  	case SetSequence(seq) =>seq
  })

  val initMatcher = new Matcher[(Symbol,InitiativeTracker)]({
  	case SetInitiative(comb,s) =>(comb,s)
  })
  
  override def setUp() {
    val context=new TrackerContext()
    val loadHandler=new TransactionalProcessor(context) with TrackerContextHandler
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    for(cid<-List('A,'B,'C,'D,'E)) {
    	loadHandler.dispatch(trans1,request.AddCombatant(new CombatantTemplate("Comb"+cid.name,10,5,CombatantType.Monster){id=cid.name}))
    }
    trans1.commit(trans1pub)
    
    //Setup the test subjects
    mockTracker=new TrackerMockup(new TrackerController(context) with ActionRecorder  {
      //addHandler(new TrackerEffectHandler(context))
      val processor= new TransactionalProcessor[TrackerContext](context) with InitiativeActionHandler with ActionAccumulator

      addPublisher(new InitiativeChangePublisher(context))
      
      def actionsExecuted:List[TransactionalAction]= processor.actionsProcessed
    }) with ActionRecorder {
      def actionsExecuted:List[TransactionalAction] = this.controller.asInstanceOf[ActionRecorder].actionsExecuted
    }
  }
  
  def testStartCombat() {
	mockTracker.dispatch(request.StartCombat(Seq('A,'B,'C,'D)))
	val s1 = extractCombatSequence()
	val ai1= extractCombatantInitiatives()
	assert(s1==List('A,'B,'C,'D,'E),s1)
	val binit=InitiativeTracker(0,InitiativeState.Waiting)
	assert(ai1.contains('A,binit))
	assert(ai1.contains('B,binit))
	assert(ai1.contains('C,binit))
	assert(ai1.contains('D,binit))
  }
  
  def testEndCombat() {
    testStartCombat()
  }
  
  /**
   * Delay combatant, move some rounds up, then jump guy in front
   */
  def testDelay() {
	testStartCombat()
	mockTracker.dispatch(request.Delay('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Delaying)))
	val s1=extractCombatSequence()
	assert(s1==List('B,'C,'D,'A,'E))
	
	startRound('B)
	endRound('B,List('C,'D,'A,'B,'E))
 
	mockTracker.dispatch(request.MoveUp('A))
	val ci4=extractCombatantInitiatives()
	assert(ci4.contains('A,InitiativeTracker(1,Acting)))
	val s4=extractCombatSequence()
	assert(s4==List('A,'C,'D,'B,'E))
 
  }
  
  def testReadyAction() {
	testStartCombat()

	startRound('A)
 
	mockTracker.dispatch(request.Ready('A))
	val ci2=extractCombatantInitiatives()
	assert(ci2.contains('A,InitiativeTracker(1,Ready)))
	assert(extractCombatSequence()==List('B,'C,'D,'A,'E))
 
	startRound('B)
 	endRound('B,List('C,'D,'A,'B,'E))

 	startRound('C)
 	mockTracker.dispatch(request.ExecuteReady('A))
	val ci3=extractCombatantInitiatives()
	assert(ci3==List(('A,InitiativeTracker(1,Waiting))),ci3)
	assert(extractCombatSequence()==List('C,'D,'B,'A,'E))
  }
  
  def testAutoStartDead() {
    import InitiativeTracker.actions._
    testStartCombat()
    
    // Kill a bunch
    {
      implicit val trans=new Transaction()
      for(x<-List('B,'C)) {
    	val cmb=mockTracker.controller.context.map(x)
        cmb.health=cmb.health.applyDamage(20)
      }
      trans.commit(null)
    }

    mockTracker.dispatch(request.StartRound('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Acting)))
	sequenceUnchanged()

	mockTracker.dispatch(request.EndRound('A))
	val ci3=extractCombatantInitiatives()
	assert(ci3.contains('A,InitiativeTracker(1,Waiting)))
	assert(ci3.contains('B,InitiativeTracker(1,Waiting)))
	assert(ci3.contains('C,InitiativeTracker(1,Waiting)))
	val s3=extractCombatSequence()
	assert(s3==List('D,'A,'B,'C,'E))
    
	// Make sure that actions are called internally
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('A,EndRound),('B,StartRound),('B,EndRound),('C,StartRound),('C,EndRound)))
  }

  def startRound(who:Symbol) {
	val init=mockTracker.controller.context.map(who).it.value
	mockTracker.dispatch(request.StartRound(who))
	val ci=extractCombatantInitiatives()
	assert(ci.contains(who,init.transform(true,InitiativeTracker.actions.StartRound)))
	sequenceUnchanged()
  } 
  
  def endRound(who:Symbol,nseq:Seq[Symbol]) {
	val init=mockTracker.controller.context.map(who).it.value
	mockTracker.dispatch(request.EndRound(who))
	val ci=extractCombatantInitiatives()
	assert(ci.contains(who,init.transform(true,InitiativeTracker.actions.EndRound)))
	val s2=extractCombatSequence()
	assert(s2==nseq)
  } 
  
  def sequenceUnchanged() {
    mockTracker.lastChangeMessages match {
      case seqMatcher.findAll() => assert(true)
      case seqMatcher.findAll(x @ _*) => assert(false, "should not have changed sequence")
    }
  }
  
  def extractCombatSequence() = {
    mockTracker.lastChangeMessages match {
      case seqMatcher.findAll(iseq) => iseq
      case _ => 
        assert(false,"Cant get new sequence list for matcher "+seqMatcher)
        Nil
    }
  }
  
  def extractCombatantInitiatives():Seq[(Symbol,InitiativeTracker)] = {
    mockTracker.lastChangeMessages match {
      case initMatcher.findAll(iseq @ _*) => iseq
    }
  }
}
