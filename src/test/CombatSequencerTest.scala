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

import junit.framework.TestCase

import vcc.model.CombatSequencer
import vcc.controller.transaction.ChangeNotification

class CombatSequencerTest extends TestCase {

  var seq:CombatSequencer[_]=null
  
  implicit val trans= new vcc.controller.transaction.Transaction
  
  
  override def setUp():Unit = {
    seq=new CombatSequencer[ChangeNotification]( x => null)
    for(x<-List('a,'b,'c,'d,'e,'f)) seq add x
    for(x<-List('c,'d)) seq moveDown x
    for(x<-List('b,'a)) seq moveUp x
  }
  
  override def tearDown():Unit = {
    seq=null
  }
  
  def testAdd {
    var seq=new CombatSequencer[ChangeNotification]( x => null)
    
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
    var seq1=new CombatSequencer[ChangeNotification]( x => null)
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
  
  def testRemoveFromList() {
    seq.removeFromSequence(List('a,'e))
    assert(seq.sequence==List('b,'c,'d,'f))
    assert(seq.reserve.toList == List('f))
  }
}
