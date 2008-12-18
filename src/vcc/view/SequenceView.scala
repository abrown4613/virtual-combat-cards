package vcc.view

trait SequenceView[T] {
  def updateSequence(seq:Seq[T]):Unit
}
