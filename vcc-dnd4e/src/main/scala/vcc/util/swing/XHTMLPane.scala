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
package vcc.util.swing

import scala.swing._

import org.w3c.dom._
import javax.xml.parsers._

import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}

import java.io.{StringReader}
import org.xml.sax.InputSource
import javax.swing.ScrollPaneConstants

object XHTMLPane {
  private val logger = org.slf4j.LoggerFactory.getLogger("fs-agent")

  private val dbfac = DocumentBuilderFactory.newInstance()

  val blankDocument = parsePanelDocument("<html><body></body></html>")
  val errorDocument = parsePanelDocument("<html><body>Failed load information. Check logs</body></html>")

  if (blankDocument == null || errorDocument == null) {
    logger.error("XHTMLPane initialization failed to create default documents")
    exit
  }

  /**
   * This is a helper function to parse String into valid
   * XHTMLPane documents.
   * @param docString A Document that must be in XML format
   * @return A valid Document or null is something failed
   */
  def parsePanelDocument(docString: String): Document = {

    val builder = dbfac.newDocumentBuilder()
    try {
      builder.parse(new InputSource(new StringReader(docString)))
    } catch {
      case e =>
        logger.debug("Failed to parse document: {}")
        logger.warn("Parsing of document failed", e)
        null
    }
  }
}

class XHTMLPane extends Component {
  override lazy val peer = new FSScrollPane()

  private val xpanel = new XHTMLPanel(XHTMLPaneAgent.getInstance)

  peer.setViewportView(xpanel)
  peer.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
  peer.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)

  xpanel.getSharedContext().getTextRenderer.setSmoothingThreshold(0)

  def setDocumentFromText(str: String) {
    val doc = if (str == "" || str == null) XHTMLPane.blankDocument
    else XHTMLPane.parsePanelDocument(str)
    if (doc != null) xpanel.setDocument(doc)
    else xpanel.setDocument(XHTMLPane.errorDocument)
  }

  def setDocument(doc: Document) {
    xpanel.setDocument(doc)
  }
}
