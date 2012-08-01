/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package org.exnebula.metric

import xml.Node
import java.net.{HttpURLConnection, URL}
import java.io.DataOutputStream
import java.util.UUID

trait MetricReporter extends {
  def  buildMessage(uuid: UUID, map: Map[String, String]): Node
  def sendMetric(xml: Node): (Int, String)
}

object MetricReporter extends MetricReporter {
  private val metricURL = new URL("http://www.exnebula.org/api/metric")

  def buildMessage(uuid: UUID, map: Map[String, String]): Node = {
    <sample id={uuid.toString}>{for ((k, v) <- map) yield <metric id={k} value={v} />}</sample>
  }

  /**
   * Send a metric document to the metric collector
   * @param xml Metric information
   * @return Pair os HTTP Return code and message
   */
  def sendMetric(xml: Node): (Int, String) = {
    val conn = openPostConnectionWithContent(metricURL, "text/xml")
    setOutboundContent(conn, xml.toString())
    (conn.getResponseCode, conn.getResponseMessage)
  }

  private def setOutboundContent(conn: HttpURLConnection, content: String) {
    val out = new DataOutputStream(conn.getOutputStream)
    out.write(content.getBytes("UTF-8"))
    out.flush()
    out.close()
  }

  private def openPostConnectionWithContent(url: URL, contentType: String) = {
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("POST")
    conn.setRequestProperty("Content-Type", contentType)
    conn.setDoInput(true)
    conn.setDoOutput(true)
    conn
  }
}