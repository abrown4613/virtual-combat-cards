/*
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
package vcc.app.dndi

import javax.servlet.http.HttpServlet
import vcc.domain.dndi._

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


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
    logger.debug("Request: {}", request.toString)


    val captureAll = System.getProperty("vcc.dndi.captureall") != null
    val result = DNDInsiderCapture.captureEntry(request.getInputStream, captureAll, captureAll, true)

    result match {
      case Some(Left((clazz, id))) =>
        response.getWriter.printf("VCC cannot capture '%s' entries yet.", clazz)
      case Some(Right(dObject)) =>
        logger.info("Captured '{}': {}", dObject.clazz, dObject("base:name").get)
        logger.debug("Catured {} is: {}", dObject.clazz, dObject)
        response.getWriter().printf("Captured %s: %s", dObject.clazz, dObject("base:name").get);
      case None =>
        response.getWriter.println("You sent something that VCC cannot capture.")
    }
  }
}
