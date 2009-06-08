//$Id$
package vcc.util

import scala.actors.Actor
import java.io.File
import java.net.URL


object Downloader {
  case class DownloadActor(actor:Actor)
  case class Progress(progress:Int, total:Int)
  case class Cancel()
  case class Failed(msg:String)
}

/**
 * This class creates a thread that sends messages to an actor during the
 * download process. Once the download is complete the actor is destroyed.
 * A special cancel message can be sent to the download thread to cancel the
 * download, in which case the partially downloaded file will be deleted.
 */
class Downloader(val url:URL, val destFile:File) extends Runnable {

  private val dThread = new Thread(this)
  private var _peer:Actor=null
  
  def start(peer: Actor) {
    _peer= peer
    dThread.start()
  }
  
  def run() {
    var downloaded=0
    val MAX_BUFFER_SIZE=1024
    
    // Tell my peer of my thread's actor
    _peer ! Downloader.DownloadActor(Actor.self)
    
    var file:java.io.RandomAccessFile  = null
    var stream:java.io.InputStream  = null
    var completed = false
    try {
      // Open connection to URL.
      val connection = url.openConnection().asInstanceOf[java.net.HttpURLConnection];
            
      // Specify what portion of file to download.
      connection.setRequestProperty("Range","bytes=" + 0 + "-");
            
      // Connect to server.
      connection.connect();
            
      // Make sure response code is in the 200 range.
      if (connection.getResponseCode() / 100 != 2) {
    	  _peer ! Downloader.Failed("Unexpected response code: "+connection.getResponseCode)
      }
            
      // Check for valid content length.
      val contentLength = connection.getContentLength()
      if (contentLength < 1) {
    	  _peer ! Downloader.Failed("Bad length for request")
      }
            
      /* Set the size for this download if it
         hasn't been already set. */
      val size = contentLength;
            	
      // Open file and seek to the end of it.
      file = new java.io.RandomAccessFile(destFile, "rw");
      file.seek(downloaded);
            
      stream = connection.getInputStream();
      var stop=false
      var lastReport= -1;
      while (downloaded < size && !stop ) {
        /* Size buffer according to how much of the
           file is left to download. */
        val buffer = new Array[Byte](Math.min(size - downloaded, MAX_BUFFER_SIZE)) 
                
        if(Actor.self.mailboxSize > 0) {
          Actor.receive {
            case Downloader.Cancel() => stop=true
          }
        }
        // Read from server into buffer.
        val read = stream.read(buffer);
        if (read == -1) { stop=true}
        else {
          // Write buffer to file.
          file.write(buffer, 0, read)
          downloaded += read

          // Report
          // TODO: Time bound reports
          val pct=((downloaded.toDouble / size.toDouble)*100).toInt
          if(lastReport < pct && downloaded != size  ) {
        	lastReport=pct
        	_peer ! Downloader.Progress(downloaded,size)
          }
        }
      }
      //COmplete sent it out.
      if(downloaded==size) {
    	_peer ! Downloader.Progress(downloaded,size)
    	completed = true
      }	
      else { 
        _peer ! Downloader.Cancel()
      }
    } catch {
      case e => _peer ! Downloader.Failed(e.getMessage);
    } 

    // Close file.
    if (file != null) {
      try {
    	file.close()
    	if(!completed) destFile.delete() //Remove uncomplete file
      } catch {
      	case e => _peer ! Downloader.Failed(e.getMessage)
      }
    }
    
    // Close connection to server.
    if (stream != null) {
      try {
    	stream.close();
      } catch {
        case e :Exception => {}
      }
    }
  }
}
