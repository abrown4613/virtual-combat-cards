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

import org.mortbay.jetty.Server
import org.mortbay.jetty.bio.SocketConnector
import org.mortbay.jetty.servlet.ServletHandler


class WebServer(port:Int) {
  
  private val server:Server = new Server()
  private val handler= new ServletHandler()
  
  protected def initialize() {
	val connector = new SocketConnector()
	connector.setPort(port)
	server.setConnectors(Array(connector))
	server.addHandler(handler)
  }
  initialize()
  
  def running:Boolean = false 
  
  def start() {
	server.start()
  }
  
  def stop() {
    server.stop() 
  }
  
  def registerServlet(classPath:String,path:String) {
    handler.addServletWithMapping(classPath,path)
  }
  
  
}
