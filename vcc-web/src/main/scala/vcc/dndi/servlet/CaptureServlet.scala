/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dndi.servlet

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import java.io.InputStream

trait CaptureService {

  def captureEntry(is: InputStream): CaptureService.Result
}

object CaptureService {
  type Result = Option[Either[(String, Int), CapturedObject]]

  private var service: CaptureService = null

  def setService(service: CaptureService) {
    this.service = service
  }

  def getInstance() = service
}

case class CapturedObject(clazz: String, name: String, obj: AnyRef)

class CaptureServlet extends HttpServlet {
  private val logger = org.slf4j.LoggerFactory.getLogger("app")

  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    response.setContentType("text/html")
    response.setStatus(HttpServletResponse.SC_OK)

    logger.debug("Request: " + request)
    val hasQuery = request.getParameter("has")
    if (hasQuery == null) {
      response.getWriter.println("<html><h1>D&D Insider Capture</h1>" +
        "<p>This page should be used with the D&D Insider Capture Firefox plugin.</p></html>")
    } else {
      response.getWriter.print(if (hasQuery == "reply-text") "true" else "false")
    }
  }

  private def humanReply(result: CaptureService.Result, response: HttpServletResponse) {
    result match {
      case Some(Left((clazz, -1))) =>
        response.getWriter.printf("VCC cannot capture '%s' entries yet.", clazz)
      case Some(Left((clazz, id))) =>
        response.getWriter.printf("VCC failed to capture '%s' with id=%s, please report.", clazz, id.toString)
      case Some(Right(dObject)) =>
        response.getWriter.printf("Captured %s: %s", dObject.clazz, dObject.name)
      case None =>
        response.getWriter.println("You sent something that VCC cannot capture.")
    }
  }

  private def pluginReply(result: CaptureService.Result, response: HttpServletResponse) {
    result match {
      case Some(Left((clazz, -1))) =>
        response.getWriter.printf("FATAL: Unknown entry type '%s'", clazz)
      case Some(Left((clazz, id))) =>
        response.getWriter.printf("Error: Failed capture of '%s' with id=%s.", clazz, id.toString)
      case Some(Right(dObject)) =>
        response.getWriter.printf("Success: %s (%s)", dObject.name, dObject.clazz)
      case None =>
        response.getWriter.println("FATAL: Bad Request.")
    }
  }

  override protected def doPost(request: HttpServletRequest, response: HttpServletResponse) {
    response.setContentType("text/html")
    response.setStatus(HttpServletResponse.SC_OK)
    logger.debug("Request: {}", request.toString)

    val result = CaptureService.getInstance().captureEntry(request.getInputStream)

    //Send output
    request.getParameter("reply") match {
      case "plugin-text" => pluginReply(result, response)
      case _ => humanReply(result, response)
    }

    // Add lag to be nice on eventual automation
    Thread.sleep(delayInterval())

    result match {
      case Some(Right(dObject)) =>
        logger.info("Captured '{}': {}", dObject.clazz, dObject.name)
        logger.debug("Catured {} is: {}", dObject.clazz, dObject)
      case _ =>
        logger.warn("Capture failed.")
    }
  }

  private object MatchInt {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: Throwable => None
    }
  }

  private def delayInterval(): Long = {
    System.getProperty("vcc.dndi.delay") match {
      case MatchInt(r) => 1000 + scala.util.Random.nextInt(r)
      case null => 0
      case s => 1000
    }
  }
}