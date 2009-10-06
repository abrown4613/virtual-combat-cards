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
import vcc.dnd4e.controller._
import vcc.controller.TrackerController
import vcc.controller.TransactionalProcessor
import vcc.controller.transaction._
import vcc.dnd4e.controller._ 

import test.helper._

import vcc.dnd4e.model.Effect._

class EffectHandlerTest extends TestCase {
  
  var mockTracker:TrackerMockup[TrackerContext]=null
  
  override def setUp() {
    val context=new TrackerContext()
    val loadHandler=new TransactionalProcessor(context) with TrackerContextHandler
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    val fighter = new CombatantEntity(null,"Figher",CharacterHealthDefinition(40,10,8),5,CombatantType.Character,null) 
    val monster = new CombatantEntity(null,"Monster",MonsterHealthDefinition(80,20,1),5,CombatantType.Monster,null) 
    val warlord = new CombatantEntity(null,"Warlord",CharacterHealthDefinition(35,8,6),5,CombatantType.Character,null) 
    
    val cl = Seq(
      request.CombatantDefinition('A,null,CombatantRepository.registerEntity(fighter)),
      request.CombatantDefinition(null,null,CombatantRepository.registerEntity(monster)),
      request.CombatantDefinition('B,null,CombatantRepository.registerEntity(warlord)))

    loadHandler.dispatch(trans1,request.AddCombatants(cl))
    
    trans1.commit(trans1pub)
    assert(trans1pub.set.contains(vcc.dnd4e.view.actor.SetSequence(List('A,Symbol("1"),'B))))
    
    //Setup the test subjects
    mockTracker=new TrackerMockup(new TrackerController(context) {
      val processor= new TransactionalProcessor[TrackerContext](context) with TrackerEffectHandler
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
    var effListExtractor=makeEffectsNameExtractor('A)
    assert(mockTracker!=null)
    
    //Load first effect 
    val ef1=Effect(src,Condition.Mark(src,false),false,Effect.Duration.EndOfEncounter)
    mockTracker.dispatch(request.AddEffect('A,ef1))
    val AaAE1=extractSingleEffectListOrFail(effListExtractor)
    val EaAE1=List("Marked by 1:EoE")
    listMustContainOnly(AaAE1,EaAE1)
    
    val ef2=Effect(src,Condition.Generic("slowed"),false,Effect.Duration.EndOfEncounter)
    mockTracker.dispatch(request.AddEffect('A,ef2))
    assert(mockTracker.lastChangeMessages != Nil,mockTracker.lastChangeMessages)
    
    val AaAE2=extractSingleEffectListOrFail(effListExtractor)
    val EaAE2=List("Marked by 1:EoE","slowed:EoE")
    listMustContainOnly(AaAE2,EaAE2)
  }

  
  /**
   * Test removal of effect from the list
   */
  def testCancelEffect() {
    val src=Symbol("1")
    loadEffect('A, Seq(
      Effect(src,Condition.Generic("ef1"),false,Duration.Other),
      Effect(src,Condition.Generic("ef2"),false,Duration.Other),
      Effect(src,Condition.Generic("ef3"),false,Duration.Other)
    ))
    
    val aeffs=makeEffectsNameExtractor('A)
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
  
  /**
   * Marks have special handling only one should be present, and then 
   * if that is a unreplaceble it should stay
   */
  def testAddingMarks() {
    val elndA=makeEffectsNameExtractor('A)
    loadEffect('A, Seq(
      Effect('B,Condition.Generic("gen"),false,Duration.SaveEnd),
      Effect('B,Condition.Mark('C,false),false,Duration.Other)))
    val elndA.findAll(eol) = mockTracker.lastChangeMessages
    assert(eol.length==2)
    
    // Add new Mark
    val EaM2=List("Marked by D:Other", "gen:SE")
    mockTracker.dispatch(request.AddEffect('A,Effect('D,Condition.Mark('D,false),false,Duration.Other)))
    val AaM2=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AaM2,EaM2)

    // Add new Mark, permanent mark
    val EaM3=List("Marked by E no mark can supersede:Other", "gen:SE")
    mockTracker.dispatch(request.AddEffect('A,Effect('E,Condition.Mark('E,true),false,Duration.Other)))
    val AaM3=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AaM3,EaM3)

    // Add new Mark, but it loses to permanent mark
    mockTracker.dispatch(request.AddEffect('A,Effect('F,Condition.Mark('F,true),false,Duration.Other)))
    assert(mockTracker.lastChangeMessages==Nil)
  }
  
  /**
   * Stance have special handling, you can only have one stance
   */
  def testAddingStance() {
    val elndA=makeEffectsNameExtractor('A)
    loadEffect('A, Seq(
      Effect('A,Condition.Generic("ef2"),false,Duration.Other),
      Effect('A,Condition.Generic("cat"),false,Duration.Stance)))
    val elndA.findAll(eol) = mockTracker.lastChangeMessages
    listMustContainOnly(eol,List("ef2:Other","cat:Stance"))
    
    // Add new Mark
    val EaM2=List("dog:Stance","ef2:Other")
    mockTracker.dispatch(request.AddEffect('A,Effect('A,Condition.Generic("dog"),false,Duration.Stance)))
    val AaM2=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AaM2,EaM2)
  }
  
  /**
   * This test must show how time passes on the effect of all combatants
   * it should indicate that time will elapse and remove effects that
   * expired.
   * Tests:
   * - RoundBound durations
   * - Stance 
   * - EoE duration
   * Should not affect other durations
   */
  def testRoundBoundEffect() {
    import Duration._
    val elndA=makeEffectNameDurationExtrator('A)
    val elndB=makeEffectNameDurationExtrator('B)
    //All effect are bound to B but affect A and B
    loadEffect('A, Seq(
      Effect('B,Condition.Generic("efrb"),false,Duration.RoundBound('B,Duration.Limit.EndOfNextTurn,false)),
      Effect('B,Condition.Generic("efsrb"),false,Duration.RoundBound('B,Duration.Limit.StartOfNextTurn,false)),
      Effect('B,Condition.Generic("efeoe"),false,Duration.EndOfEncounter),
      Effect('B,Condition.Generic("efese"),false,Duration.SaveEnd),
      Effect('B,Condition.Generic("efese*"),false,Duration.SaveEndSpecial),
      Effect('B,Condition.Generic("efestn"),false,Duration.Stance)
    ))
    loadEffect('B, Seq(
      Effect('B,Condition.Generic("efrb"),false,Duration.RoundBound('B,Duration.Limit.EndOfNextTurn,false)),
      Effect('B,Condition.Generic("efsrb"),false,Duration.RoundBound('B,Duration.Limit.StartOfNextTurn,false)),
      Effect('B,Condition.Generic("efeoe"),false,Duration.EndOfEncounter),
      Effect('B,Condition.Generic("efese"),false,Duration.SaveEnd),
      Effect('B,Condition.Generic("efese*"),false,Duration.SaveEndSpecial),
      Effect('B,Condition.Generic("efestn"),false,Duration.Stance)
    ))
    //println(mockTracker.lastChangeMessages)
    var elndB.findFirst(x)=mockTracker.lastChangeMessages
    // Just for sanity
    listMustContainOnly(x,List(("efestn",Stance), ("efese*",SaveEndSpecial), ("efese",SaveEnd), ("efeoe",EndOfEncounter), ("efsrb",RoundBound('B,Limit.StartOfNextTurn,false)), ("efrb",RoundBound('B,Limit.EndOfNextTurn,false)))    )
    
    //Must process start of round for B
    mockTracker.dispatch(request.InternalInitiativeAction(mockTracker.controller.context.map('B),InitiativeTracker.actions.StartRound))
    val AaSR=extractSingleEffectListOrFail(elndA)
    val BaSR=extractSingleEffectListOrFail(elndB)
    //println("A->"+AaSR)
    //println("B->"+BaSR)
    val EaSR=List(("efestn",Stance), ("efese*",SaveEndSpecial), ("efese",SaveEnd), ("efeoe",EndOfEncounter), ("efrb",RoundBound('B,Limit.EndOfTurn,false)))
    //println("E->"+EaSR)
    listMustContainOnly(AaSR,EaSR)
    listMustContainOnly(BaSR,EaSR)
    
    // End round of B
    mockTracker.dispatch(request.InternalInitiativeAction(mockTracker.controller.context.map('B),InitiativeTracker.actions.EndRound))
    val AaER=extractSingleEffectListOrFail(elndA)
    val BaER=extractSingleEffectListOrFail(elndB)
    //println("A->"+AaSR)
    //println("B->"+BaSR)
    val EaER=List(("efestn",Stance), ("efese*",SaveEndSpecial), ("efese",SaveEnd), ("efeoe",EndOfEncounter))
    //println("E->"+EaSR)
    listMustContainOnly(AaER,EaER)
    listMustContainOnly(BaER,EaER)
    
    //End Combat does not cause changes
    mockTracker.dispatch(request.EndCombat())
    assert(mockTracker.lastChangeMessages==Nil)
    
    mockTracker.dispatch(request.ApplyRest(false))
    val EaR=List(("efese*",SaveEndSpecial), ("efese",SaveEnd))
    val AaR=extractSingleEffectListOrFail(elndA)
    val BaR=extractSingleEffectListOrFail(elndB)
    listMustContainOnly(AaR,EaR)
    listMustContainOnly(BaR,EaR)
  }
  
  /**
   * Shoud that sustaining will bounce duration up
   */
  def testSustain() {
    import Duration._
    val elndA=makeEffectNameDurationExtrator('A)
    //All effect are bound to B but affect A and B
    loadEffect('A, Seq(
      Effect('A,Condition.Generic("nsus"),false,RoundBound('A,Limit.EndOfNextTurn,false)),
      Effect('A,Condition.Generic("sus"),false,RoundBound('A,Limit.EndOfNextTurn,true))
    ))
    //Sustain a effect that is on boundary, should not change duration
    mockTracker.dispatch(request.SustainEffect('A,0))
    assert(mockTracker.lastChangeMessages==Nil)
    
    val EaSR=List(("sus",RoundBound('A,Limit.EndOfTurn,true)), ("nsus",RoundBound('A,Limit.EndOfTurn,false)))
    
    // Start round, and make these ready for sustain
    mockTracker.dispatch(request.InternalInitiativeAction(mockTracker.controller.context.map('A),InitiativeTracker.actions.StartRound))
    val AaSR=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AaSR,EaSR)

    // Start round, and make these ready for sustain
    val EaS0=List(("sus",RoundBound('A,Limit.EndOfNextTurn,true)), ("nsus",RoundBound('A,Limit.EndOfTurn,false)))
    mockTracker.dispatch(request.SustainEffect('A,0))
    val AaS0=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AaS0,EaS0)

    //Sustain unsustainable should result in no change
    mockTracker.dispatch(request.SustainEffect('A,1))
    assert(mockTracker.lastChangeMessages==Nil)

  }
  
  
  /**
   * Delay is a complex action, must have the following behavior:
   * Process a startRound for the delaying
   * Process a special endRound were:
   *  - all effect that are benefial and would end and the end of the round are taken out
   *  - Sustains all are NOT sustained
   * 
   */
  def testDelay() {
    import Duration._
    val src=Symbol(1.toString)
    val elnd1=makeEffectsNameExtractor(src)
    val elndA=makeEffectsNameExtractor('A)
    //All effect are bound to B but affect A and B
    loadEffect(src, Seq(
      Effect('B,Condition.Generic("bad"),false,Duration.RoundBound('B,Limit.EndOfTurn,false)),
      Effect('B,Condition.Generic("good"),true,Duration.RoundBound('B,Limit.EndOfTurn,false)),
      Effect('B,Condition.Generic("goodsus"),true,Duration.RoundBound('B,Limit.EndOfTurn,true))
    ))
    var elnd1.findFirst(y)=mockTracker.lastChangeMessages
    // Just for sanity
    listMustContainOnly(y,List("goodsus:EoT*:B", "good:EoT:B", "bad:EoT:B"))

    loadEffect('A, Seq(
      Effect('B,Condition.Generic("bad"),false,Duration.RoundBound('B,Limit.EndOfTurn,false)),
      Effect('B,Condition.Generic("good"),true,Duration.RoundBound('B,Limit.EndOfTurn,false)),
      Effect('B,Condition.Generic("goodsus"),true,Duration.RoundBound('B,Limit.EndOfTurn,true))
    ))
    //println(mockTracker.lastChangeMessages)
    var elndA.findFirst(x)=mockTracker.lastChangeMessages
    // Just for sanity
    listMustContainOnly(x,List("goodsus:EoT*:B", "good:EoT:B", "bad:EoT:B"))

    mockTracker.dispatch(request.InternalInitiativeAction(mockTracker.controller.context.map('B),InitiativeTracker.actions.Delay))
    val MaD=extractSingleEffectListOrFail(elnd1)
    listMustContainOnly(MaD,List("good:EoT:B") )
    val BaD=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(BaD,List("bad:EoT:B") ) // Only bad stuff
  }

  /**
   * You can only update generic effects. So we add some effects than attempt to 
   * update the generic ones
   */
  def testUpdateEffectText() {
    val elndA=makeEffectsNameExtractor('A)
    loadEffect('A, Seq(
      Effect('B,Condition.Mark('C,false),false,Duration.Other),
      Effect('B,Condition.Generic("ef1"),false,Duration.SaveEnd),
      Effect('B,Condition.Generic("ef2"),false,Duration.SaveEndSpecial)))
    val elndA.findAll(eol) = mockTracker.lastChangeMessages

    
    //This is the actual order in this version
    val BE=List("ef2:SE*","ef1:SE","Marked by C:Other")
    listMustContainOnly(eol,BE)
    
    //Update effect 0: ef2
    mockTracker.dispatch(request.UpdateEffect('A,0,Condition.Generic("new ef2")))
    val UEL1=List("ef1:SE","new ef2:SE*","Marked by C:Other")
    val AEL1=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AEL1,UEL1)
    
    //Update effect 1: ef1
    mockTracker.dispatch(request.UpdateEffect('A,1,Condition.Generic("new ef1")))
    val UEL2=List("new ef1:SE","new ef2:SE*","Marked by C:Other")
    val AEL2=extractSingleEffectListOrFail(elndA)
    listMustContainOnly(AEL2,UEL2)

    
    //Update effect 2: A mark, this must not change any value
    mockTracker.dispatch(request.UpdateEffect('A,2,Condition.Generic("new mark")))
    assert(mockTracker.lastChangeMessages==Nil)
  }
  
  //UTILITIES 
  
  def makeEffectsNameExtractor(on:Symbol) = {
    new test.helper.Matcher[List[String]]({
      case response.UpdateEffects(`on`,el) =>
          el.map(e=>e.condition.description+":"+e.duration.shortDesc)
    })
  }
  
  def makeEffectListExtractor(on:Symbol) = {
    new test.helper.Matcher[List[Effect]]({
      case response.UpdateEffects(`on`,el) => el
    })
  }
  
  def makeEffectNameDurationExtrator(on:Symbol) = {
    new Matcher[List[(String,Duration)]]({
      case response.UpdateEffects(`on`,el) => el.map(eff=>(eff.condition.description,eff.duration))
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
 
  /**
   * Load a list of effect to combatatn
   * @param to Combatant to add effect to
   * @param effects List of effects to add
   */
  def loadEffect(to:Symbol,effects:Seq[Effect]) {
    val toelm= new test.helper.Matcher[List[Effect]]({ case response.UpdateEffects(`to`,el) => el })
    for(eff<-effects) {
      mockTracker.dispatch(request.AddEffect(to,eff))
      mockTracker.lastChangeMessages match {
        case toelm.findAll(el) if(el.contains(eff)) => 
        case _ => 
          //println(mockTracker.lastChangeMessages)
          //println(toelm.findAll.unapplySeq(mockTracker.lastChangeMessages))
          assert(false,"Effect not in effect list")
      }
    }
  }
}