//$Id$
package vcc.util.swing

import scala.swing._
import javax.swing.JDialog

class ModalDialog (owner: Frame, title:String) extends UIElement with RootPanel with Publisher
{
  override lazy val peer = new JDialog(owner.peer,title,true)
}
