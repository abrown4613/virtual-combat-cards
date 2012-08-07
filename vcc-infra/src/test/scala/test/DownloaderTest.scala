/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package test

import junit.framework.TestCase
import vcc.util.Downloader
import java.net.URL
import scala.actors.Actor
import org.junit.Ignore

class DownloaderTest extends TestCase {
  
  def peerActor(parent: Actor, f: PartialFunction[Any, Boolean]) = Actor.actor {
    var running=true;
    var trouble:Exception = null
    while(running) {
      try {
    	running= Actor.receiveWithin(1000)(f)
      } catch {
        case e:Exception => 
          trouble=e
          running=false
      }
    }
    if(trouble!=null) parent ! trouble
    else parent ! 'DONE
  }
  
  def nottestDownloadBadURL {
    // Flush messages 

    val file=java.io.File.createTempFile("vcc",".zip")
	val down=new Downloader(new URL("http://127.0.0.1/a.zip"), file);
    down.start(peerActor(Actor.self,{
      case Downloader.Failed(msg) => 
          false // End run correctly
        case Downloader.DownloadActor(actor) => 
          true
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case s => 
          println("Got :: "+s)
          throw new Exception("This should not be reached")
          false
      }))
    
    Actor.receive {
      case 'DONE => assert(true)
      case e:Exception => assert(false,e)
    }
    file.delete()
  }


  def testDummy() {
    assert(true)
  }
  
  @Ignore
  def nottestXML {
    val file=java.io.File.createTempFile("vcc",".xml")
	val down=new Downloader(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"), file);
    down.start(peerActor(Actor.self,{
       case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
       case Downloader.Failed(msg) => 
         throw new Exception("Got a message: "+ msg) 
          false
        case Downloader.DownloadActor(actor) => 
          true
        case Downloader.Progress(down,total) if(down==total)=>
          if(down==total) println("COmpleted!")
          false
        case Downloader.Progress(down,total) =>
          println("Progress :"+down+"/"+total)
          true
        case s => 
          println("Got : "+s)
          throw new Exception("This should not be reached"+s)
      }))
    Actor.receive {
      case 'DONE => assert(true)
      case e:Exception => assert(false,e)
    }
    //file.delete()
    println(file)
  }

  @Ignore
  def nottestZip {
    val file=java.io.File.createTempFile("vcc",".zip")
	val down=new Downloader(new URL("http://www.exnebula.org/files/vcc/vcc-0.10.zip"), file);
    down.start(peerActor(Actor.self,{
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case Downloader.Failed(msg) => 
          throw new Exception("Got a message "+msg)
          false
        case Downloader.DownloadActor(actor) => 
          true
        case Downloader.Progress(down,total) if(down==total)=>
          if(down==total) println("COmpleted!")
          false
        case Downloader.Progress(down,total) =>
          println(Thread.currentThread + "Progress :"+down+"/"+total)
          true
        case s => 
          println("Got : "+s)
          throw new Exception("This should not be reached"+s)
      }))
    Actor.receive {
      case 'DONE => assert(true)
      case e:Exception => assert(false,e)
    }
    //file.delete()
    println(file)
  }

  @Ignore
  def nottestCancelZip {
    val file=java.io.File.createTempFile("vcc",".zip")
	val down=new Downloader(new URL("http://www.exnebula.org/files/vcc/vcc-0.10.zip"), file);
 
    var dActor:Actor = null
    down.start(peerActor(Actor.self,{
        case scala.actors.TIMEOUT =>
          throw new Exception("Timeout, should not get here")
          false
        case Downloader.Failed(msg) => 
          throw new Exception("Got a message "+msg)
          false
        case Downloader.DownloadActor(actor) => 
          dActor=actor;
          Thread.sleep(50)
          dActor ! Downloader.Cancel()
          true
        case Downloader.Progress(down,total) if(down==total)=>
          throw new Exception("Should have cancelled")
          false
        case Downloader.Progress(down,total) =>
          println(Thread.currentThread + "Progress :"+down+"/"+total)
          true
        case Downloader.Cancel() =>
          false
        case s => 
          println("Got : "+s)
          throw new Exception("This should not be reached"+s)
      }))
    
    
    Actor.receive {
      case 'DONE => assert(true)
      case e:Exception => assert(false,e)
    }
    //file.delete()
    println(file)
  }

}
