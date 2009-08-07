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

package vcc.dndi

import javax.servlet.ServletException
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServlet


class CaptureServlet extends HttpServlet {

  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse) {
	response.setContentType("text/html");
	response.setStatus(HttpServletResponse.SC_OK);
	response.getWriter().println("<h1>Hello SimpleServlet</h1>");
  }		

  override protected def doPost(request: HttpServletRequest, response: HttpServletResponse) {
	response.setContentType("text/html");
	response.setStatus(HttpServletResponse.SC_OK);
	println(request.toString)
	val xml=try {
	  scala.xml.XML.load(request.getInputStream)
    } catch {
      case s => throw s
    }  
    try {
      println("I got his XMl"+xml)
      val monster=vcc.dndi.DNDInsiderCapture.load(xml)
      monster match {
        case s: vcc.dndi.Monster => 
          println("Read monster "+monster("NAME"))
          println("     monster "+monster("INITIATIVE"))
          println("     monster "+monster("HP"))
          println("Monster "+monster)
          response.getWriter().println("Captured monster "+monster("NAME").get);
        case null =>
          response.getWriter.println("You sent something that is no capturable")
      }
    } catch {
      case ue: vcc.dndi.UntranslatableException => 
        // XML is ok, but can be parsed so we save it for debuging
        val file=java.io.File.createTempFile("capture", ".xml")
        println(ue)
        ue.printStackTrace
        println("Write to file"+file.getAbsolutePath)
        scala.xml.XML.save(file.getAbsolutePath,xml,"UTF-8")
        response.getWriter().println("Could not process capture correctly. Save XML to "+file.getAbsoluteFile+ ". If you thing this should be captured, report an bug on http://www.exnebula.org/vcc and send us the file")
    }
  }		
}
