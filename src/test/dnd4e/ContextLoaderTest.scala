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
package test.dnd4e
import junit.framework.TestCase
import vcc.dnd4e.model._
import vcc.controller.transaction._
import vcc.dnd4e.controller._
import vcc.controller.{TransactionalProcessor,CommandSource}
import vcc.dnd4e.controller.request._

class ContextLoaderTest extends TestCase {
  
  var context=new TrackerContext()
  var source:CommandSource = null
  
  def testLoadCombatant {
    //TODO: This is not the real way to do it, much better, but needs improvement
    val handler=new TransactionalProcessor(context) with TrackerContextHandler
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    val fighter = CombatantEntity(null,"Fighter",new CharacterHealthDefinition(40,10,5),5,CombatantType.Character,null)
    val monster = CombatantEntity(null,"Monster",new MonsterHealthDefinition(80,20,1),4,CombatantType.Monster,null)
    val fighterID = CombatantRepository.registerEntity(fighter)
    val monsterID = CombatantRepository.registerEntity(monster)

    assert(fighterID != null)
    assert(monsterID != null)
    
    handler.dispatch(trans1,source,request.AddCombatants(List(CombatantDefinition('A,null,fighterID),CombatantDefinition(null,null,monsterID))))
    
    trans1.commit(trans1pub)
    //FIXME assert(trans1pub.set.contains(CombatSequenceChanged(List('A,Symbol("1")))))
  }

  import vcc.controller.Tracker
  import vcc.controller.transaction.ChangeNotification
  import vcc.controller.message._

  def testRealTracker() {
    val tracker = Tracker.initialize(new vcc.controller.TrackerController(context) {
      val processor = new TransactionalProcessor(context) with TrackerContextHandler
      def publish(changes:Seq[ChangeNotification]):Any = {
      }
    })
    assert(tracker != null)
    val fighter = CombatantEntity(null,"Fighter",new CharacterHealthDefinition(40,10,5),5,CombatantType.Character,null)
    val fighterID = CombatantRepository.registerEntity(fighter)
    
    //tracker ! Command(null,request.AddCombatants(List(CombatantDefinition('A,null,fighterID))))
    new TestCommandSource(tracker) onComplete {
      changes => 
      	println("Called me with changes "+changes)
        Thread.sleep(500)
        //assert(false,"Failed because I want to")
    } onCancel {
      reason =>
      	println("Cancelled with reason")
    } dispatch(request.AddCombatants(List(CombatantDefinition('A,null,fighterID))))
  }
}

import vcc.controller.Tracker
import vcc.controller.message.{TransactionalAction,Command}
import scala.actors.Actor._
import scala.actors.Actor
import scala.actors.OutputChannel

class TestCommandSource(val tracker:Tracker) extends CommandSource {

  val core = actor {
    var run = true
    var from:OutputChannel[Any]= null
    while(run) {
      println("Running ....")
      receive {
        case c:Command =>
          from = sender
          println("From : "+from)
          tracker ! c
          println("Sent "+c)
        case ('COMPLETE,msg) => 
          println("Here")
          from ! msg
          run = false
        case ('CANCEL,reason) =>
          from ! reason
          run = false
      }
    }
    println("Exiting source actor")
  }
  
  private var completeBlock:Seq[ChangeNotification]=>Unit = null
  private var cancelBlock:String=>Unit = null
  
  def onComplete(block: Seq[ChangeNotification]=>Unit):TestCommandSource = {
    completeBlock = block
    this
  }

  def onCancel(block: String=>Unit):TestCommandSource = {
    cancelBlock = block
    this
  }
  
  def dispatch(action:TransactionalAction):Any = {
    println("Sending "+action)
    val r = core !? Command(this,action)
    println("Reply was " + r)
    r
  }
  
  def actionCancelled(reason:String) {
    if(cancelBlock != null) cancelBlock(reason)
    core ! ('CANCEL,reason)
  }
  
  def actionCompleted(msg:String) {
    println("Complete called")
    if(completeBlock != null) completeBlock(Nil)
    core ! ('COMPLETE,msg)
  }
  
}

class SetChangePublisher extends TransactionChangePublisher {
  val set=scala.collection.mutable.Set.empty[ChangeNotification]
  def publishChange(seq:Seq[ChangeNotification]) {
    for(c<-seq) set+=c
  }
}

