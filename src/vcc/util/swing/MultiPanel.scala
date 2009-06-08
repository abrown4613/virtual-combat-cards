//$Id$
package vcc.util.swing

import scala.swing._
import vcc.util.swing.multipanel._

import java.io.File
import java.net.URL

import scala.actors.Actor.{receive}

trait MultiPanel extends RootPanel {

  private val controller = new Controller(this)
  private val selfActor = scala.actors.Actor.self
  controller.start()
  
  def showMessage(wait:Boolean,msg:String) {
    val p=new InformationPanel(wait,msg)
    controller ! SetPanel(selfActor,p)
    if(wait) receive { case s => p.returnHandler(s)}
  }
  
  def downloadFile(url:URL,target:File):File = {
    val panel=new DownloadPanel(url,target)
    controller ! SetPanel(selfActor,panel)
    receive { case s => panel.returnHandler(s) }
  }
  
  /**
   * Set a generic panel
   * @param panel An AbstractPanel that will be used as a control
   * @return The value of what handler did
   */
  def customPanel[T](panel:AbstractPanel[T]):T = {
    controller ! SetPanel(selfActor,panel)
    receive { case s => panel.returnHandler(s) }
  }
}
