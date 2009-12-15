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

package vcc.app.dndi

import javax.servlet.http.HttpServlet;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


import vcc.domain.dndi.DNDInsiderCapture
import vcc.domain.dndi.UntranslatableException
import vcc.domain.dndi.Monster

class CaptureServlet extends HttpServlet {
  
  val logger = org.slf4j.LoggerFactory.getLogger("app")

  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse) {
	response.setContentType("text/html");
	response.setStatus(HttpServletResponse.SC_OK);
	response.getWriter().println("<html><h1>D&D Insider Capture</h1><p>This page should be used with the D&D Insider Capture Firefox plugin.</p></html>");
  }		


  override protected def doPost(request: HttpServletRequest, response: HttpServletResponse) {
	response.setContentType("text/html");
	response.setStatus(HttpServletResponse.SC_OK);
	logger.debug("Request: {}",request.toString)
	val xmlRaw = DNDInsiderCapture.pluginInputStreamAsFilteredString(request.getInputStream)
	logger.debug("Raw stream data: "+xmlRaw)
	val xml=try {
	  scala.xml.XML.loadString(xmlRaw)
    } catch {
      case s =>
        logger.warn("Failed to parse XML",s)
        logger.debug("XML Raw: {}",xmlRaw)
        throw s
    }
 
 
    if(System.getProperty("vcc.dndi.captureall") != null) {
      val otype = DNDInsiderCapture.getTypeFromXML(xml)
      val oid = DNDInsiderCapture.getIdFromXML(xml)
      if(otype.isDefined && oid.isDefined) {
    	CaptureStore.storeObject(otype.get,oid.get,vcc.dnd4e.Configuration.baseDirectory.value,xml)
      }
      
    }
 
    try {
      logger.debug("Parsed XML is: {}",xml)
      val dndiObject = DNDInsiderCapture.load(xml)
      dndiObject match {
        case monster: Monster => 
          logger.info("Captured monster {}",monster("NAME"))
          logger.debug("Catured Monster: {}",monster)
          CaptureHoldingArea.addCapturedMonsterAndCacheXML(monster,xml)
          //val log = org.mortbay.log.Logger.getLogger(null)
          response.getWriter().println("Captured monster "+monster("NAME").get);
        case null =>
          val otype = DNDInsiderCapture.getTypeFromXML(xml)
          if(otype.isDefined) response.getWriter.println("VCC cannot capture '"+ otype.get + "' entries yet.")
          else response.getWriter.println("You sent something that VCC cannot capture.")
      }
    } catch {
      case ue: Exception => 
        // XML is ok, but can be parsed so we save it for debuging
        logger.warn("Failed to capture monster",ue)
        val file=java.io.File.createTempFile("capture", ".xml")
        logger.warn("Write to file"+file.getAbsolutePath)
        scala.xml.XML.save(file.getAbsolutePath,xml,"UTF-8")
        logger.warn("Failed capture informations saved at: {}",file.getAbsolutePath)
        response.getWriter().println("Could not process capture correctly. Save XML to "+file.getAbsoluteFile+ ". If you thing the information should be captured, report an bug on http://www.exnebula.org/vcc and send us the file.")
    }
  }		
}
