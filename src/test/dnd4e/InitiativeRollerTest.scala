//$Id$
package test.dnd4e

import junit.framework.TestCase
import vcc.dnd4e.view.dialog.InitiativeDialogEntry
import vcc.dnd4e.view.helper.InitiativeRoller
import vcc.dnd4e.view.helper.InitiativeRoller.GroupInitEntry
import vcc.util.DiceBag

class InitiativeRollerTest extends TestCase {

  //List DiceBag seed 1 =List(6, 9, 8, 14, 15, 5, 15)
  //List DiceBag seed 1 = List(true, true, true, false, false, true, false, true)
  
  //DiceBag seed 1L
  //println((1 to 7).map(_=> DiceBag.D(20)).toList)
  //println((1 to 8).map(_=> DiceBag.flipCoin).toList)

  
  var initList:List[InitiativeDialogEntry]=Nil
       
  override def setUp(){                    
    initList=List(
      new InitiativeDialogEntry('A,"Alpha",2,0,false),
      new InitiativeDialogEntry('B,"Bravo",4,0,false),
      new InitiativeDialogEntry('C,"Charlie",6,0,false),
      new InitiativeDialogEntry('W,"Goblin",3,0,false),
      new InitiativeDialogEntry('X,"Goblin",5,0,false),
      new InitiativeDialogEntry('Y,"Goblin",5,0,false),
      new InitiativeDialogEntry('Z,"Goblin",5,0,false)
    )
  }
    
  def testRollAll() {
    //var ilist=InitiativeRoller.createInitiativeGroups(false,initList)
  //List DiceBag seed 1 =List(6, 9, 8, 14, 15, 5, 15)
    /*
   8  assert(ilist.contains( GroupInitEntry(0,2,Set('A)) ))
   13  assert(ilist.contains( GroupInitEntry(0,4,Set('B)) ))
   14 assert(ilist.contains( GroupInitEntry(0,6,Set('C)) ))
   17 assert(ilist.contains( GroupInitEntry(0,3,Set('W)) ))
   20 assert(ilist.contains( GroupInitEntry(0,5,Set('X)) ))
   10 assert(ilist.contains( GroupInitEntry(0,5,Set('Y)) ))
   20 assert(ilist.contains( GroupInitEntry(0,5,Set('Z)) ))
    */
    DiceBag seed 1L
    val seq=InitiativeRoller.rollInitiative(true,initList)
    assert(seq.toList==List('C,'A,'B,'W,'X,'Y,'Z),seq.toList)
    DiceBag seed 1L
    val seq2=InitiativeRoller.rollInitiative(false,initList)
    assert(seq2==List('Z,'X,'W,'C,'B,'Y,'A),seq2)
    
  }
  def testFormGroupsOfSingleEntries() {
    var ilist=InitiativeRoller.createInitiativeGroups(false,initList)
    assert(ilist.contains( GroupInitEntry(0,2,Set('A)) ))
    assert(ilist.contains( GroupInitEntry(0,4,Set('B)) ))
    assert(ilist.contains( GroupInitEntry(0,6,Set('C)) ))
    assert(ilist.contains( GroupInitEntry(0,3,Set('W)) ))
    assert(ilist.contains( GroupInitEntry(0,5,Set('X)) ))
    assert(ilist.contains( GroupInitEntry(0,5,Set('Y)) ))
    assert(ilist.contains( GroupInitEntry(0,5,Set('Z)) ))
    
    for(i<-(0 to initList.length -1))
      initList(i).roll=5+i*2
    
    ilist=InitiativeRoller.createInitiativeGroups(false,initList)
    assert(ilist.contains( GroupInitEntry(5,2,Set('A)) ))
    assert(ilist.contains( GroupInitEntry(7,4,Set('B)) ))
    assert(ilist.contains( GroupInitEntry(9,6,Set('C)) ))
    assert(ilist.contains( GroupInitEntry(11,3,Set('W)) ))
    assert(ilist.contains( GroupInitEntry(13,5,Set('X)) ))
    assert(ilist.contains( GroupInitEntry(15,5,Set('Y)) ))
    assert(ilist.contains( GroupInitEntry(17,5,Set('Z)) ))
    
  }
  
  def testFormGroups() {
    var ilist=InitiativeRoller.createInitiativeGroups(true,initList)
    assert(ilist.contains( GroupInitEntry(0,2,Set('A)) ))
    assert(ilist.contains( GroupInitEntry(0,4,Set('B)) ))
    assert(ilist.contains( GroupInitEntry(0,6,Set('C)) ))
    assert(ilist.contains( GroupInitEntry(0,3,Set('W)) ))
    assert(ilist.contains( GroupInitEntry(0,5,Set('X,'Y,'Z)) ),ilist)

    for(i<-(0 to initList.length -1)) initList(i).roll=5+i*2
  
    ilist=InitiativeRoller.createInitiativeGroups(true,initList)
    assert(ilist.contains( GroupInitEntry(5,2,Set('A)) ))
    assert(ilist.contains( GroupInitEntry(7,4,Set('B)) ))
    assert(ilist.contains( GroupInitEntry(9,6,Set('C)) ))
    assert(ilist.contains( GroupInitEntry(11,3,Set('W)) ))
    assert(ilist.contains( GroupInitEntry(17,5,Set('X,'Y,'Z)) ),ilist)
  }  
  
  /**
   * Should not roll initiative for combatants in reserve
   */
  def testSkipReserve() {
    initList(0).reserve=true
    initList(1).reserve=true
    var ilist=InitiativeRoller.createInitiativeGroups(true,initList)
    assert(!ilist.contains( GroupInitEntry(0,2,Set('A)) ),ilist)
    assert(!ilist.contains( GroupInitEntry(0,4,Set('B)) ))
    assert(ilist.contains( GroupInitEntry(0,6,Set('C)) ))
    assert(ilist.contains( GroupInitEntry(0,3,Set('W)) ))
    assert(ilist.contains( GroupInitEntry(0,5,Set('X,'Y,'Z)) ),ilist)
  }
  
  def testDiceBagPredict() {
    DiceBag seed 1L
    
    val d11=DiceBag.D(20)
    val d12=DiceBag.D(20)
    val d13=DiceBag.D(20)
    val l1=(1 to 100).map(_=> DiceBag.D(20)).toList
    
    DiceBag seed 1L
    val d21=DiceBag.D(20)
    val d22=DiceBag.D(20)
    val d23=DiceBag.D(20)
    val l2=(1 to 100).map(_=> DiceBag.D(20)).toList
    
    assert(d11==d21)
    assert(d12==d22)
    assert(d13==d23)
    assert(l2==l1)
  }
}
