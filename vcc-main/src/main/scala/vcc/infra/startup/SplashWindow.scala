/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.startup

import javax.swing._
import java.awt._
import scala.swing.{Frame=>SwingFrame}

/**
 * This is the interface to any window that will report user
 * information during startup.
 */
trait StartupReportWindow {
  
  def reportProgress(obj:AnyRef,msg:String)
  
  def ownerWindow:Window
}

/**
 * Indicates some StartupStep failed.
 */
class StartupException(msg:String) extends Exception(msg)

/**
 * Object that implements the application startup logic.
 */
trait StartupRoutine {
  
  def callStartupSimpleBlock(srw:StartupReportWindow,msg:String)(block : => Boolean) {
	srw.reportProgress(null,"Initializing "+msg)
	val ss = block
	Thread.sleep(50)
	if(ss) srw.reportProgress(null," intitialized successfully")
	else throw new StartupException(msg)
  }
  
  def callStartupStep(srw:StartupReportWindow,msg:String)(block : => StartupStep) {
	srw.reportProgress(null,"Initializing "+msg)
	val ss = block
	Thread.sleep(50)
	if(ss.isStartupComplete) srw.reportProgress(ss," intitialized successfully")
	else throw new StartupException(msg)
  }
  
  def start(srw: StartupReportWindow):SwingFrame
}

/**
 * This is a helper trait to mark initialization
 */
trait StartupStep {
  def isStartupComplete:Boolean
}

/**
 * This is a low level JWindow implementation for the Splashwindow.
 */
object SplashWindow extends JWindow with StartupReportWindow {
  
  val logger = org.slf4j.LoggerFactory.getLogger("startup")

  def reportProgress(obj:AnyRef,msg:String) {
    val cname:String = if(obj!=null) {
      val name = obj.getClass.getSimpleName.replace('$','.')
      if(name.endsWith(".")) name.substring(0,name.length-1)
      else name
    } +": " else "" 
	messageLabel.setText(cname + msg)
	logger.info(cname + msg)
  }
  
  def ownerWindow:Window = getOwner
  
  protected def loadIcon(resource:String):ImageIcon = {
	val url=this.getClass.getResource(resource)
	if(url!=null) {
	  val icon=new ImageIcon(url)
	  icon
    } else
	  vcc.infra.AbnormalEnd(this,"Failed to load resource: "+resource)
  }
  
  private val messageLabel = new JLabel()
  messageLabel setBorder BorderFactory.createLineBorder(Color.WHITE,2)
  private val content = getContentPane.asInstanceOf[JPanel]
  content.add(new JLabel(loadIcon("/vcc/infra/startup/vcc-splash.png")),BorderLayout.NORTH)
  content.add(messageLabel,BorderLayout.CENTER)
  content.setBackground(Color.WHITE)
  content.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY,1))
    
  def showSplash(sr:StartupRoutine):SwingFrame = {
	val screen = Toolkit.getDefaultToolkit.getScreenSize
	val height = 140
	val width = 400
	setBounds((screen.width - width)/2, (screen.height - height)/2,width,height)
	setVisible(true)
	val frame = try { sr.start(this) } 
		catch {
			case e:StartupException =>
			  logger.error("Failed initialization step: "+e.getMessage)
              logger.debug("Startup Exception",e)
			  null
			case e =>
			  logger.error("Failed to startup with and exception: ",e.getMessage)
              logger.debug("Startup Exception",e)
			  null
	}
	messageLabel.setText(if(frame==null) "Initialization failed..." else "Initialization complete." )
    setVisible(false)
    frame
  }
}
