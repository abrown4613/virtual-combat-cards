//$Id$
package vcc.util.swing

import scala.swing._
import javax.swing.{AbstractListModel,ComboBoxModel}

/**
 * Model for ComboBox, it allows update to the contents via properties.
 */
class ContainterComboBoxModel[A](iv:Seq[A]) extends AbstractListModel with ComboBoxModel {
  
  var entries:Seq[A]=iv
  
  private var selected:A = if(entries.isEmpty) null.asInstanceOf[A] else entries(0)
  
  def getSelectedItem: AnyRef = selected.asInstanceOf[AnyRef]
  
  def setSelectedItem(a: Any) { selected = a.asInstanceOf[A] } 
  
  def getElementAt(n: Int) = entries(n).asInstanceOf[AnyRef]
  
  def getSize = entries.length
  
  def contents_=(nv:Seq[A]) { 
    entries=nv
    fireIntervalAdded(this,0,nv.length)
  }
  
  def contents:Seq[A]=entries
  
}

/**
 * This sub class of scala.swing.ComboBox is designed to be constructed based on a model
 * and not on a list. This allows model to be changed externally.
 */
class ExplicitModelComboBox[T](mdl:javax.swing.ComboBoxModel) extends ComboBox[T](Nil) {
  import javax.swing._
  override lazy val peer: JComboBox = new JComboBox(mdl) with SuperMixin
  
}
