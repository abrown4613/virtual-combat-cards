//$Id$
package vcc.controller.transaction

/**
 * This is the base change for all notifications to be sent to transactions 
 * observers.
 */
abstract class ChangeNotification

/**
 * This trait is used to allows delayed creation or messages in the ChangeNotifiticationPromise.
 */
trait ChangeNotifier {
  /**
   * Deliver the promised notification.
   */
  def createNotification():ChangeNotification
}

/**
 * This is a special ChangeNotification used to deliver a single changes notification for 
 * classes that contain several Undoable vals. Since a single ChangeNotifier is used, 
 * transaction publishing will not send more than one message. The ChangeNotifier will be called
 * to expand the promised change into a real ChangeNotification
 */
case class ChangeNotificationPromise(cn:ChangeNotifier) extends ChangeNotification

/**
 * This is the parametric type UndoMemento, which is used to store previous states of Undoable 
 * objects.
 */
case class UndoMemento[T](val obj:Undoable[T],val value:T) {

  /**
   * Restore from mementos
   */
  def undo() {
    obj.restore(this)
  }
  /**
   * Get a redo memento, so that if you want to redo you have the original value
   */
  def redoMemento() = UndoMemento[T](obj,obj.value)
  
  /**
   * Get ChangeNotification (if there is one)
   */
  def changeNotification:Option[ChangeNotification] = if(obj.f!= null) Some(obj.f(obj)) else None
}

/**
 * This is transcation controlled field. It will store mementos of changes to it in a 
 * transactions. This allows changes to be undone or redone.
 */
class Undoable[T](initValue:T,val f:Undoable[T]=>ChangeNotification) {
  
  private var _value : T =initValue
  
  def value:T = this._value
  
  /**
   * Store new value, and save memento of last value in a Transaction
   */
  def value_=(v:T)(implicit trans:Transaction):Undoable[T] = {
    trans.addMemento(UndoMemento(this,_value))
    this._value=v
    this
  }
  
  override def toString:String = "Undoable["+_value+"]"
  
  /**
   * Restore a Memento
   */
  def restore(memento:UndoMemento[T]) {
    this._value=memento.value
  }
  
}
