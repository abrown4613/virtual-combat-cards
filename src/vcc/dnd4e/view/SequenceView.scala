//$Id$
package vcc.dnd4e.view

trait SequenceView[T] {
  def updateSequence(seq:Seq[T]):Unit
}
