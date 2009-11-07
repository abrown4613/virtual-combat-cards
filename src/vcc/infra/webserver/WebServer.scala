//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

package vcc.infra.webserver

import vcc.infra.startup.StartupStep

import org.mortbay.jetty.Server
import org.mortbay.jetty.bio.SocketConnector
import org.mortbay.jetty.servlet.ServletHandler
import org.mortbay.component._

class WebServer(port:Int) extends StartupStep {

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")
  
  protected object  ServerStatus extends LifeCycle.Listener {
	var running:Boolean = false 
 
	def lifeCycleFailure(event: LifeCycle, cause: Throwable) {
	  logger.info("Server Failed")
    }
           
    def lifeCycleStarted(event: LifeCycle) {
      logger.info("Server started")
      running = true
    }
           
    def lifeCycleStarting(event: LifeCycle) {
      logger.info("Server starting")
    }
    
    def lifeCycleStopped(event: LifeCycle) {
      logger.info("Server stopped")
      running = false
    }
           
    def lifeCycleStopping(event: LifeCycle) {
      logger.info("Server stoping")
    }
  }

  private val server = new Server()
  private val handler= new ServletHandler()
  
  protected def initialize() {
	val connector = new SocketConnector()
	connector.setPort(port)
	server.setConnectors(Array(connector))
	server.addHandler(handler)
	server.addLifeCycleListener(ServerStatus)
  }
  initialize()
  
  def isStartupComplete() = true
 
  def running = ServerStatus.running
  
  def start() {
    try {
      server.start()
    } catch {
      case e => 
        server.stop
        server.join
    }
  }
  
  def stop() {
    server.stop()
    server.join()
  }
  
  def registerServlet(classPath:String,path:String) {
    handler.addServletWithMapping(classPath,path)
  }
  
}

object WebServer {
  
  import vcc.model.Registry

  def initialize(name:String,port:Int):WebServer = {
    val ws = new vcc.infra.webserver.WebServer(4143)
    ws.registerServlet("vcc.app.dndi.CaptureServlet","/capture")
    Registry.register("webserver",ws)
    try { ws.start } catch { case s => }
    ws
  }
}
