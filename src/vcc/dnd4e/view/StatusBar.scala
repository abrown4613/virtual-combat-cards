//$Id$
package vcc.dnd4e.view

import vcc.util.swing.MigPanel
import vcc.dnd4e.BootStrap
import scala.swing._

class StatusBar(uia: scala.actors.Actor) extends MigPanel("ins 1,fillx") {
  private val versionLabel= new Label("Version: "+BootStrap.version)
  private val tipLabel = new Label("Welcome to Virtual Combat Cards") 
  tipLabel.xAlignment= Alignment.Left

  border=javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED)
 
  private val sep=new Component { override lazy val peer= new javax.swing.JSeparator(javax.swing.SwingConstants.VERTICAL)}
  add(versionLabel, "align left")
  add(sep, "growy")
  add(tipLabel,"growx,align left")
 
  /**
   * Set the text in the tip area
   * @param str Text to be set
   */
  def setTipText(str:String) {
    tipLabel.text=str
  }
}
