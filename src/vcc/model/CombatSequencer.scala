package vcc.model

class CombatSequencer[T] {
  private var _sequence:List[T]=Nil
  private var _reserve=Set.empty[T]
  
  def rotate() {
    if(!_sequence.isEmpty) _sequence = _sequence.tail ::: List(_sequence.head)
  }
  def moveUp(elem:T) {
    if(_reserve(elem)) { 
      _reserve -=elem
      _sequence= elem :: _sequence
    } else {
      _sequence= elem :: (_sequence -- List(elem))
    }                 
  }
  def moveDown(elem:T) {
    if(_reserve(elem)) {
      _reserve -=elem
      _sequence= _sequence ::: List(elem)      
    } else {
      var t=List(elem)
      _sequence= (_sequence -- t) ::: t
    }
  }
  def add(elem:T) {
    if(_sequence.contains(elem))
      _sequence=_sequence -- List(elem)
    _reserve += elem
  }
  
  def clear() {
    _sequence=Nil
    _reserve=Set.empty[T]
  }
  
  def reserve():Set[T] = _reserve
  def sequence():List[T] = _sequence ::: _reserve.toList
}
