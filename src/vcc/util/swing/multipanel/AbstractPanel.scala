//$Id$
package vcc.util.swing.multipanel

import scala.actors.Actor
import scala.swing._

trait AbstractPanel[T] extends Panel {
  
  self : Panel =>
  
  protected var remote:Actor = null
 
  def setRemote(actor:Actor) { remote=actor}
  
  def returnHandler(msg:Any):T
}