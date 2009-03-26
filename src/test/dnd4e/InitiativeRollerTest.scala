//$Id$
package test.dnd4e

import junit.framework.TestCase
import vcc.dnd4e.view.dialog.InitiativeDialogEntry
import vcc.dnd4e.view.helper.InitiativeRoller
import vcc.util.DiceBag

class InitiativeRollerTest extends TestCase {

  //List DiceBag seed 1 =List(6, 9, 8, 14, 15, 5, 15)
  //List DiceBag seed 1 = List(true, true, true, false, false, true, false, true)
 
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
    
    //DiceBag seed 1L
    //println((1 to 7).map(_=> DiceBag.D(20)).toList)
    //println((1 to 8).map(_=> DiceBag.flipCoin).toList)
  
  def testFormGroupsOfSingleEntries() {
    var ilist=InitiativeRoller.createInitiativeGroups(false,initList)
    assert(ilist.contains( (0,2,Set('A)) ))
    assert(ilist.contains( (0,4,Set('B)) ))
    assert(ilist.contains( (0,6,Set('C)) ))
    assert(ilist.contains( (0,3,Set('W)) ))
    assert(ilist.contains( (0,5,Set('X)) ))
    assert(ilist.contains( (0,5,Set('Y)) ))
    assert(ilist.contains( (0,5,Set('Z)) ))
    
    for(i<-(0 to initList.length -1))
      initList(i).roll=5+i*2
    
    ilist=InitiativeRoller.createInitiativeGroups(false,initList)
    assert(ilist.contains( (5,2,Set('A)) ))
    assert(ilist.contains( (7,4,Set('B)) ))
    assert(ilist.contains( (9,6,Set('C)) ))
    assert(ilist.contains( (11,3,Set('W)) ))
    assert(ilist.contains( (13,5,Set('X)) ))
    assert(ilist.contains( (15,5,Set('Y)) ))
    assert(ilist.contains( (17,5,Set('Z)) ))
    
  }
  
  def testFormGroups() {
    var ilist=InitiativeRoller.createInitiativeGroups(true,initList)
    assert(ilist.contains( (0,2,Set('A)) ))
    assert(ilist.contains( (0,4,Set('B)) ))
    assert(ilist.contains( (0,6,Set('C)) ))
    assert(ilist.contains( (0,3,Set('W)) ))
    assert(ilist.contains( (0,5,Set('X,'Y,'Z)) ),ilist)
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
