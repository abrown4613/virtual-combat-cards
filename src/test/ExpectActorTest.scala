package test

import junit.framework.TestCase

import scala.actors.{Actor,TIMEOUT}
import scala.actors.Actor._

class ExpectActorTest extends TestCase {
  val timeout=250
  case class MyMessage(s:Int)

  def testExpectBehavior() {
      
    var nev=new ExpectingBehavior(List(1,2))
    
    var x=nev.run(1)
    assert(x==None)
    x=nev.run(2)
    assert(x==None)
    assert(!nev.running)
  }

  def testExpectBehaviorWrongSequence() {
    var nev=new ExpectingBehavior(List(1,2,3))
    
    var x=nev.run(1)
    assert(x==None)
    x=nev.run(1)
    assert(x==Some(1))
    assert(!nev.running)
  }
  
  def testExpectNothing() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List()),self)
    var t0=System.currentTimeMillis
    
    expect.start

    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
    assert(System.currentTimeMillis-t0<25)
  }
  
  def testGetNothingButWantedSomething() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1)),self)
    var t0=System.currentTimeMillis

    expect.start
    
    receive {
      case expect.Done(s) if(s==expect)=> assert(false,"Should timeout")
      case expect.Timeout(s) if(s eq expect) => assert(true)
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
    assert(System.currentTimeMillis-t0<140)
  }

  def testGetItWithDelay() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1)),self)
    var t0=System.currentTimeMillis

    expect.start
    
    Thread.sleep(80)
    expect ! 1

    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should not timeout")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
    assert(System.currentTimeMillis-t0<140)
    Thread.sleep(timeout)
  }

  def testToLate() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1)),self)
    expect.start
    
    Thread.sleep(200)
    expect ! 1
    receive {
      case expect.Done(s) if(s==expect)=> assert(false,"Should timeout")
      case expect.Timeout(s) if(s eq expect) => assert(true)
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
  }

  def testLongSequence() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1,2,3,4,5,6)),self)
    expect.start
    

    for(x<-1 to 7 ) expect ! x 
    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should not timeout")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
  }

  
  def testWrongSequence() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1,2,3,4,5,6)),self)
    expect.start
    
    for(x<-1 to 4 ) expect ! x
    expect ! 6
    receive {
      case expect.Done(s) if(s==expect)=> assert(false,"Should send feedback")
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(true)
    }
  }

  def testComplexObject() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1,MyMessage(2),MyMessage(6))),self)
    expect.start
    
    expect ! 1
    expect ! MyMessage(2)
    expect ! MyMessage(6)
    
    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpeced feedback"+w)
    }
  }

  def testComplexObjectMismatch() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1,MyMessage(2),MyMessage(6))),self)
    expect.start
    
    expect ! 1
    expect ! MyMessage(2)
    expect ! MyMessage(7)
    
    receive {
      case expect.Done(s) if(s==expect)=> assert(false,"Should send feedback")
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(true)
    }
  }
  
  def testChangeBehavior() {
    var accum=new AccumulatingBehavior()
    val expect=new ExpectActor(100,accum,self)
    expect.start
    for(x<- 1 to 10) expect ! x
    expect ! expect.ChangeBehavior(new ExpectingBehavior(List(12,13,14)))
    for(x<- 12 to 14) expect ! x

    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpected feedback:"+w)
    }
    assert(accum.accumulated.size==10)
    
  }

  def testAccumStopBehavior() {
    var accum=new AccumulatingBehavior()
    val expect=new ExpectActor(100,accum,self)
    expect.start
    for(x<- 1 to 10) expect ! x
    expect ! expect.ChangeBehavior(new ExpectingBehavior(Nil))

    receive {
      case expect.Done(s) if(s==expect)=> assert(true)
      case expect.Timeout(s) if(s eq expect) => assert(false,"Should termintate")
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpected feedback:"+w)
    }
    assert(accum.accumulated.size==10)
    
  }
  
  
  def testWainForActorEnd() {
    val expect=new ExpectActor(100,new ExpectingBehavior(List(1,2,3)),self)
    expect.start
    
    var trickme= actor {
      receive {
        case s:Actor => reply(expect.Done(s))
      }
    }
    expect ! 1
    Thread.sleep(50)
    trickme ! trickme
    expect ! 2
    
    receive {
      case expect.Done(s) if(s eq expect) => assert(false,"Can't complete on incomplete sequence") //Should not complete
      case expect.Timeout(s) if(s eq expect) => assert(true)                                                          
      case expect.Feedback(s,w) if(s eq expect) => assert(false,"Unexpected feedback:"+w)
    }
    receive {
      case expect.Done(s) if(s eq trickme) => assert(true)
      case s => 
        println("Other stuff"+s)
        assert(false)
    }
  }
  
}

