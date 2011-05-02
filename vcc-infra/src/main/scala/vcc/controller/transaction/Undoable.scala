/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.controller.transaction

/**
 * This is the base change for all notifications to be sent to transactions 
 * observers.
 */
abstract class ChangeNotification

/**
 * This trait is used to allows delayed creation or messages in the ChangeNotificationPromise.
 */
trait ChangeNotifier {
  /**
   * Deliver the promised notification.
   */
  def createNotification(): ChangeNotification
}

/**
 * This is a special ChangeNotification used to deliver a single changes notification for 
 * classes that contain several Undoable vals. Since a single ChangeNotifier is used, 
 * transaction publishing will not send more than one message. The ChangeNotifier will be called
 * to expand the promised change into a real ChangeNotification
 */
case class ChangeNotificationPromise(cn: ChangeNotifier) extends ChangeNotification

/**
 * This is the parametric type UndoMemento, which is used to store previous states of Undoable 
 * objects.
 */
case class UndoMemento[T](obj: Undoable[T], value: T) {

  /**
   * Restore from mementos
   */
  def undo() {
    obj.restore(this)
  }

  /**
   * Get a redo memento, so that if you want to redo you have the original value
   */
  def redoMemento() = UndoMemento[T](obj, obj.value)

  /**
   * Get ChangeNotification for this memento.
   * @return None if the re is not createChange function or that function returned null. Some[ChangeNotification]
   * otherwise.
   */
  def changeNotification: Option[ChangeNotification] = {
    if (obj.createChange != null) {
      val change = obj.createChange(obj, value)
      if (change == null) None else Some(change)
    }
    else None
  }
}

/**
 * This is transcation controlled field. It will store mementos of changes to it in a 
 * transactions. This allows changes to be undone or redone.
 */
class Undoable[T](initValue: T, val createChange: (Undoable[T], T) => ChangeNotification) {

  def this(initValue: T, createChange: Undoable[T] => ChangeNotification) = this (initValue, (u: Undoable[T], v: T) => createChange(u))

  def this(initValue: T) = this (initValue, null.asInstanceOf[(Undoable[T], T) => ChangeNotification])

  private var _value: T = initValue

  def value: T = this._value

  /**
   * Store new value, and save memento of last value in a Transaction
   */
  def value_=(v: T)(implicit trans: Transaction): Undoable[T] = {
    trans.addMemento(UndoMemento(this, _value))
    this._value = v
    this
  }

  override def toString: String = "Undoable[" + _value + "]"

  /**
   * Restore a Memento
   */
  def restore(memento: UndoMemento[T]) {
    this._value = memento.value
  }

}

/**
 * This Undoable mixin allows user to add code to notify other object that it's is about to change. It will call the
 * restoreCallback function prior to restoring the value. This means that the current value is available as is the value
 * that will be restored.
 */
trait UndoableWithCallback[T] extends Undoable[T] {
  override def restore(memento: UndoMemento[T]) {
    restoreCallback(memento.value)
    super.restore(memento)
  }

  /**
   * This function will be called prior to restoring the value from the memento. Current <code>value</code> contains the
   * current value being held at the Undoable.
   * @param valueToRestore The value that is stored in the memento.
   */
  def restoreCallback(valueToRestore: T)
}