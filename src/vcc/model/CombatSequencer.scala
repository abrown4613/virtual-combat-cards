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
package vcc.model

import vcc.controller.transaction._

/**
 * Control the sequence of combatants, includes a list of combatant in sequence
 * and a set of Combatant in reserve.
 */
class CombatSequencer extends ChangeNotifier {
  type T=Symbol
  private var _sequence= new Undoable[List[T]](Nil,x=>ChangeNotificationPromise(this))
  private var _reserve= new Undoable[Set[T]](Set.empty[T],x=>ChangeNotificationPromise(this))
  
  def createNotification():ChangeNotification = {
    vcc.dnd4e.view.actor.SetSequence(this.sequence.toSeq)
  }
  
  
  /**
   * Move to combatant to end of sequence, just before reserve
   */
  def rotate()(implicit trans:Transaction) {
    if(!_sequence.value.isEmpty){
      var l=_sequence.value
      l = l.tail ::: List(l.head)
      _sequence.value=l
    }
  }
  
  /**
   * Move elem from current position to top of sequence.
   */
  def moveUp(elem:T)(implicit trans:Transaction) {
    if(_reserve.value(elem)) { 
      _reserve.value -= elem
      _sequence.value = elem :: _sequence.value
    } else {
      _sequence.value = elem :: (_sequence.value -- List(elem))
    }                 
  }
  
  /**
   * Move elem to bottom of sequence, just before reserve
   */
  def moveDown(elem:T)(implicit trans:Transaction) {
    if(_reserve.value(elem)) {
      _reserve.value -= elem
      _sequence.value = _sequence.value ::: List(elem)      
    } else {
      var t=List(elem)
      _sequence.value = (_sequence.value -- t) ::: t
    }
  }
  
  /**
   * Move element 'elem' to a position before 'before'. Elements cant be in the reserve, 
   * can't be the same element and must be in the list
   * @param elem The element to move
   * @param before The element before with elem must be placed
   */
  def moveBefore(elem:T,before:T)(implicit trans:Transaction) {
    if(elem != before && !_reserve.value(elem) && !_reserve.value(before) && _sequence.value.contains(elem) && _sequence.value.contains(before)) {
      val elist=List(elem)
      val lst=_sequence.value -- elist
      val (lh,lr)=lst.splitAt(lst.indexOf(before))
      _sequence.value=lh:::List(elem):::lr
    }
  }
  
  /**
   * Add an combatant to the reserve.
   */
  def add(elem:T)(implicit trans:Transaction) {
    if(_sequence.value.contains(elem))
      _sequence.value=_sequence.value -- List(elem)
    _reserve.value += elem
  }
  
  /**
   * Clear the entire list of all elements 
   */
  def clear()(implicit trans:Transaction) {
    _sequence.value =Nil
    _reserve.value =Set.empty[T]
  }
  
  /**
   * Removes  a list of elements form the sequence and reserve in a single
   * operation
   */
  def removeFromSequence(rlst:List[T])(implicit trans:Transaction) {
    _sequence.value = _sequence.value -- rlst
    _reserve.value = _reserve.value -- rlst
  } 
    
  def reserve():Set[T] = _reserve.value
  def sequence():List[T] = _sequence.value ::: _reserve.value.toList
}
