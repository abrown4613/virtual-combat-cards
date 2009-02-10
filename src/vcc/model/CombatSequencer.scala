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
