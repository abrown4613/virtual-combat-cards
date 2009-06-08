//$Id$
package vcc.util.swing.multipanel

import scala.swing._
import scala.swing.event._
import vcc.util.UpdateManager.Release

class ReleaseSelectPanel(releases:Seq[(Symbol,Release)]) extends MigPanel("") with AbstractPanel[Option[Release]] {  
  //Buttons
  private val okButton= new Button("Update")
  private val cancelButton= new Button("Cancel")
  private val infoButton= new Button("Information")

  private val symbolMeaning = Map(
    'RC -> "Release Candidate, may be unstable",
    'UPGRADE -> "Upgrade includes new features",
    'PATCH -> "Patch to current version"
  ).withDefaultValue("?")
  okButton.enabled = false
  infoButton.enabled = false

  val opts=releases.map (x=> new RadioButton(x._2.version.versionString + "(" + symbolMeaning(x._1)+ ")"))
  val radios=new ButtonGroup(opts : _*);
  var selected = -1

  add(new Label("Select the version:"),"wrap")
  for(x <- opts) {
	add(x,"wrap")
	listenTo(x)
  }
  
  add(okButton, "split 4")
  add(infoButton)
  add(cancelButton)
  listenTo(okButton)
  listenTo(infoButton)
  listenTo(cancelButton)
  
  reactions += {
    case ButtonClicked(this.okButton) => 
      remote ! releases(selected)._2
      
    case ButtonClicked(this.cancelButton) => 
      remote ! None
      
    case ButtonClicked(this.infoButton) => 
      SwingHelper.openDesktopBrowser(releases(selected)._2.info)
      
    case ButtonClicked(button) => 
      selected= opts.indexOf(button)
      okButton.enabled = (selected >= 0)
      infoButton.enabled = (selected >= 0)
  }

  def returnHandler(msg:Any):Option[Release] = {
    msg match {
      case r: Release => Some(r)
      case None => None
      case s => 
        println("Custome Panel release" + s) 
        None
    }
  }

}

