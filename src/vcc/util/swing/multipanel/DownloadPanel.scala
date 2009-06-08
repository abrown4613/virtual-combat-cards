//$Id$
package vcc.util.swing.multipanel

import scala.swing._
import scala.swing.event._
import java.net.URL
import java.io.File
import scala.actors.Actor
import scala.actors.Actor.{actor,loop,react,receive}
import vcc.util.Downloader

class DownloadPanel(url:URL, targetFile:File) extends MigPanel("") with AbstractPanel[File] {
  add(new Label("Downloading "+url),"wrap")
  private val progress=new ProgressBar()
  private val cancelButton=new Button("Cancel")
  private val kbytesCount= new Label("0/0")

  private var dActor: scala.actors.Actor = null
  add(progress,"wrap,align center")
  add(kbytesCount,"wrap,align center")
  add(cancelButton,"wrap,align center")
    
  listenTo(cancelButton)
    
  val observer= actor {
    var running=true
	while(running) {
	  receive {
	    case Downloader.Progress(current, total) if(current==total)=>
	      remote ! 'COMPLETE
	      running=false
	    case Downloader.Progress(current, total) =>
	      progress.value=(100.0 * (current.toDouble / total.toDouble)).toInt
	      kbytesCount.text = current + "/"+total +" bytes"
	    case Downloader.Cancel() => 
          remote ! 'CANCEL
          running = false
	    case Downloader.DownloadActor(actor) =>
	      dActor=actor
	    case Downloader.Failed(msg) =>
	      remote ! msg
	      running = false
	    case s =>
	      println(s)
	  }
	}
  }

  val downloader=new Downloader(url,targetFile)
  
  //Only Start download when I have the actor set
  override def setRemote(actor:Actor) {
    super.setRemote(actor)
	downloader.start(observer)  
  }
  
  
  reactions += {
    case b:ButtonClicked => 
    	println(b)
      	dActor ! Downloader.Cancel()
  }

  def returnHandler(msg:Any):File = 
    msg match {
      case 'COMPLETE => targetFile
      case 'CANCEL => null
      case s => 
        println("Failed with: "+s)
      	null
    }
}
