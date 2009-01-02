//$Id$
package vcc.model

/**
 * Control the sequence of combatants, includes a list of combatant in sequence
 * and a set of Combatant in reserve.
 */
class CombatSequencer[T] {
  private var _sequence:List[T]=Nil
  private var _reserve=Set.empty[T]
  
  /**
   * Move to combatant to end of sequence, just before reserve
   */
  def rotate() {
    if(!_sequence.isEmpty) _sequence = _sequence.tail ::: List(_sequence.head)
  }
  
  /**
   * Move elem from current position to top of sequence.
   */
  def moveUp(elem:T) {
    if(_reserve(elem)) { 
      _reserve -= elem
      _sequence= elem :: _sequence
    } else {
      _sequence= elem :: (_sequence -- List(elem))
    }                 
  }
  
  /**
   * Move elem to bottom of sequence, just before reserve
   */
  def moveDown(elem:T) {
    if(_reserve(elem)) {
      _reserve -= elem
      _sequence= _sequence ::: List(elem)      
    } else {
      var t=List(elem)
      _sequence= (_sequence -- t) ::: t
    }
  }
  
  /**
   * Add an combatant to the reserve.
   */
  def add(elem:T) {
    if(_sequence.contains(elem))
      _sequence=_sequence -- List(elem)
    _reserve += elem
  }
  
  /**
   * Clear the entire list of all elements 
   */
  def clear() {
    _sequence=Nil
    _reserve=Set.empty[T]
  }
  
  /**
   * Removes  a list of elements form the sequence and reserve in a single
   * operation
   */
  def removeFromSequence(rlst:List[T]) {
    _sequence= _sequence -- rlst
    _reserve = _reserve -- rlst
  } 
    
  def reserve():Set[T] = _reserve
  def sequence():List[T] = _sequence ::: _reserve.toList
}
