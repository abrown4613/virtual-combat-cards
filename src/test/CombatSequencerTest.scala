package test

import junit.framework.TestCase

import vcc.model.CombatSequencer

class CombatSequencerTest extends TestCase {

  var seq:CombatSequencer[Symbol]=null
  
  
  override def setUp():Unit = {
    seq=new CombatSequencer
    for(x<-List('a,'b,'c,'d,'e,'f)) seq add x
    for(x<-List('c,'d)) seq moveDown x
    for(x<-List('b,'a)) seq moveUp x
  }
  
  override def tearDown():Unit = {
    seq=null
  }
  
  def testAdd {
    var seq=new CombatSequencer[Symbol]
    
    assert(seq.sequence==Nil)
    assert(seq.reserve==Set.empty[Symbol])
    
    var l=List('a,'b,'c,'d)
    for(x<-l) seq add x
    for(x<-l) {
      assert(seq.reserve.contains(x),"Reserve is missing "+x)
    }
    seq moveUp 'b
    assert((seq.sequence -- seq.reserve.toList)==List('b))
    assert(!seq.reserve.contains('b))
    seq moveUp 'a
    assert((seq.sequence -- seq.reserve.toList)==List('a,'b))
    assert(!seq.reserve.contains('a))
    seq moveDown 'c
    assert((seq.sequence -- seq.reserve.toList)==List('a,'b,'c))
    assert(!seq.reserve.contains('c))
    assert(seq.reserve.contains('d))
  }
  
  def testSetupCRIT {
    // Sanity check for the setUP method
    var l=List('a,'b,'c,'d)
    for(x<-l) {
      assert(!seq.reserve.contains(x),"Reserve is missing "+x)
    }
    assert((seq.sequence -- seq.reserve.toList)==List('a,'b,'c,'d),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
    assert(seq.reserve.contains('e))
    assert(seq.reserve.contains('f))
  }
  
  def testRotate() {
    var seq1=new CombatSequencer
    
    // Should work 
    seq1.rotate 
    assert(seq1.sequence==Nil, "Wrong: "+seq1.sequence)
    
    seq.rotate
    assert((seq.sequence -- seq.reserve.toList)==List('b,'c,'d,'a),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
    seq.rotate
    assert((seq.sequence -- seq.reserve.toList)==List('c,'d,'a,'b),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
    seq.rotate
    assert((seq.sequence -- seq.reserve.toList)==List('d,'a,'b,'c),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
    seq.rotate
    assert((seq.sequence -- seq.reserve.toList)==List('a,'b,'c,'d),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
  }
  
  def testMoveUpInSequence() {
    seq moveUp 'c
    assert((seq.sequence -- seq.reserve.toList)==List('c,'a,'b,'d),"Wrong: "+(seq.sequence -- seq.reserve.toList))

    seq moveUp 'd
    assert((seq.sequence -- seq.reserve.toList)==List('d,'c,'a,'b),"Wrong: "+(seq.sequence -- seq.reserve.toList))
    
    seq moveUp 'd
    assert((seq.sequence -- seq.reserve.toList)==List('d,'c,'a,'b),"Wrong: "+(seq.sequence -- seq.reserve.toList))    
  }

  def testMoveDownInSequence() {
    seq moveDown 'a
    assert((seq.sequence -- seq.reserve.toList)==List('b,'c,'d,'a),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))

    seq moveDown 'c
    assert((seq.sequence -- seq.reserve.toList)==List('b,'d,'a,'c),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
    
    seq moveDown 'c
    assert((seq.sequence -- seq.reserve.toList)==List('b,'d,'a,'c),"Wrong sequence: "+(seq.sequence -- seq.reserve.toList))
  }
  
  def testBackToReserve() {
    seq add 'a
    //Start of slit must not contain 'a
    var head=(seq.sequence.splitAt(3)._1)
    assert(head==List('b,'c,'d),"Wrong sequence: "+head)
    assert(seq.reserve.contains('a))
    seq add 'c
    head=(seq.sequence.splitAt(2)._1)
    assert(head==List('b,'d),"Wrong sequence: "+head)
    assert(seq.reserve.contains('c))
    seq.rotate()
    head=(seq.sequence.splitAt(2)._1)
    assert(head==List('d,'b),"Wrong sequence: "+head)
  }

  def testClear() {
    seq.clear
    
    assert(seq.sequence==Nil)
    assert(seq.reserve.toList==Nil)
  }
}
