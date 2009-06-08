//$Id$
package vcc.util.swing.multipanel

import scala.actors.Actor
import scala.actors.Actor._
import scala.swing._

case class SetPanel[T](remote:Actor, panel: AbstractPanel[T])

class Controller(multiPanel: MultiPanel) extends Actor {

  def act () {
    loop {
      react {
        case SetPanel(remote,panel) =>
          panel.setRemote(remote)
          multiPanel.contents = panel
      }
    }
  }
}
