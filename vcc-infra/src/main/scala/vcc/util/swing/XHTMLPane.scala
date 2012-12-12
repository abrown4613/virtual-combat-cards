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
package vcc.util.swing

import scala.swing._

import org.w3c.dom._
import javax.xml.parsers._

import java.io.StringReader
import org.xml.sax.InputSource
import javax.swing.ScrollPaneConstants
import org.xhtmlrenderer.extend.UserAgentCallback
import org.xhtmlrenderer.simple.{XHTMLPanel, FSScrollPane}
import collection.JavaConversions
import org.xhtmlrenderer.swing.{LinkListener, FSMouseListener}

object XHTMLPane {
  private val logger = org.slf4j.LoggerFactory.getLogger("fs-agent")

  private val dbfac = DocumentBuilderFactory.newInstance()

  val blankDocument = parsePanelDocument("<html><body></body></html>")
  val errorDocument = parsePanelDocument("<html><body>Failed load information. Check logs</body></html>")

  if (blankDocument == null || errorDocument == null) {
    logger.error("XHTMLPane initialization failed to create default documents")
    sys.exit(2)
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
      case e: Exception =>
        logger.debug("Failed to parse document: {}")
        logger.warn("Parsing of document failed", e)
        null
    }
  }
}

/**
 * Create XHTMLPane using a custom UserAgentCallback.
 * @param uac Agent callback to use.
 */
class XHTMLPane(uac: UserAgentCallback) extends Component {
  override lazy val peer = new FSScrollPane()

  /**
   * Create XHTMLPane with default XHTMLPaneAgent agent callback
   */
  def this() = this (XHTMLPaneAgent.getInstance())


  private val xPanel = new XHTMLPanel(uac)

  peer.setViewportView(xPanel)
  peer.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
  peer.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)

  xPanel.getSharedContext.getTextRenderer.setSmoothingThreshold(0)

  /**
   * Remove all the event MouseTrackerListeners from this pane.
   */
  def removeAllMouseTrackerListeners() {
    val ml = JavaConversions.asScalaBuffer(xPanel.getMouseTrackingListeners.asInstanceOf[java.util.List[FSMouseListener]])
    for (l <- ml) {
      xPanel.removeMouseTrackingListener(l)
    }
  }

  /**
   * Remove only the LinkListener from the list, this keeps hover and pointer behaviour change.
   */
  def removeLinkListener() {
    val ml = JavaConversions.asScalaBuffer(xPanel.getMouseTrackingListeners.asInstanceOf[java.util.List[FSMouseListener]])
    for (l <- ml) {
      if (l.isInstanceOf[LinkListener])
        xPanel.removeMouseTrackingListener(l)
    }
  }

  def addMouseTrackingListener(listener: FSMouseListener) {
    xPanel.addMouseTrackingListener(listener)
  }

  /**
   * Set XML document.
   * @param str String version of the XML document.
   */
  def setDocumentFromText(str: String) {
    val doc = if (str == "" || str == null) XHTMLPane.blankDocument
    else XHTMLPane.parsePanelDocument(str)
    if (doc != null) xPanel.setDocument(doc)
    else xPanel.setDocument(XHTMLPane.errorDocument)
  }

  /**
   * Set document of pane
   * @param doc DOM version of the document
   */
  def setDocument(doc: Document) {
    xPanel.setDocument(doc)
  }
}