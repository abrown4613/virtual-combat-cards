//$Id$
package vcc.util.swing
import scala.swing._
import net.miginfocom.swing._

class MigPanel(layoutConstrains:String, colConstriants:String, rowConstraints:String) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = new javax.swing.JPanel(new MigLayout(layoutConstrains,colConstriants,rowConstraints))

  def this(layoutContraints:String) = this(layoutContraints,"","")
  
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  
  protected def add(c: Component, l: String) { peer.add(c.peer, l) }
}
