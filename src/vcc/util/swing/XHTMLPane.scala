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
package vcc.util.swing

import scala.swing._

import org.w3c.dom._
import javax.xml.parsers._

import org.xhtmlrenderer.resource._
import org.xhtmlrenderer.swing.AWTFSImage
import org.xhtmlrenderer.simple.XHTMLPanel 

import javax.swing.ImageIcon
import java.io.File
import vcc.infra.startup.StartupStep

object XHTMLPane extends StartupStep {

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")

  private val dbfac = DocumentBuilderFactory.newInstance()
 
  val blankDocument = parsePanelDocument("<html><body></body></html>")
  val errorDocument = parsePanelDocument("<html><body>Failed load information. Check logs</body></html>")
  
  if(blankDocument == null || errorDocument == null) {
    logger.error("XHTMLPane initialization failed to create default documents")
    exit
  }
  
  private val missingIcon:ImageIcon = {
    val url = this.getClass.getResource("/vcc/util/swing/missing.png")
    try {
      val icon=new javax.swing.ImageIcon(url)
      if(icon == null) throw new Exception("Can't load image "+url.toString)
      icon
    } catch {
      case s =>
        s.printStackTrace
        null
    }
  }
  
  protected object LocalUserAgent extends org.xhtmlrenderer.swing.NaiveUserAgent {

    
  private val imageCache = new XHTMLPane.ContentCache[ImageResource](
    new ImageResource(AWTFSImage.createImage(missingIcon.getImage)),
    uri =>{
      logger.debug("SF-UA Requested load of image: {}",uri)
      val iname = uri.substring(uri.lastIndexOf('/')+1).toLowerCase
      val file = new File("fs-wc/images",iname)
      logger.debug("SF-UA Image {} mapped to {}",uri,file)
      if(file.exists && file.canRead && file.isFile) {
    	try { 
    	   new ImageResource(AWTFSImage.createImage((new ImageIcon(file.toURL)).getImage))
    	} catch { 
           case e =>
             logger.warn("SF-UA Failed to load file "+file,e)
             null
    	}
      } else {
        logger.warn("SF-UA File {} not found or not readable",file)
    	null 
      }
    })
  

    override def getImageResource(uri:String):ImageResource = {
      imageCache.getObject(uri)
    } 
    
    override def getCSSResource(uri:String):CSSResource = {
      logger.debug("SF-UA request CSSResource for URI: {}",uri)
      val file = new java.io.File("fs-wc/css",uri)
      logger.debug("SF-UA URI {} mapped to file {}",uri,file)
      if(file.exists && file.canRead && file.isFile) {
        logger.debug("SF-UA Reading CSS file: {}",file)
        new CSSResource(new java.io.FileInputStream(file))
      } else {
        logger.warn("SF-UA  CSS file: {} is not readable or does not exists, sending blank",file)
        new CSSResource(new java.io.ByteArrayInputStream("".getBytes))
      }
    }
    
    override def resolveURI(uri:String):String = {
      logger.debug("SF-UA request resolveURI: {}",uri)
      super.resolveURI(uri)
      val nuri = if(uri.startsWith("http://")) null
      else uri // Avoid relative URL that do not math this when base URL is not set
      logger.debug("SF-UA Mapped to URI: {}",nuri)
      nuri
    }
  }

  class ContentCache[T](default:T, fetcher: String=>T) {
    private var cache = Map.empty[String,T]
  
    def getObject(uri:String):T = {
      if(cache.isDefinedAt(uri)) cache(uri)
      else {
        var o = fetcher(uri)
        if(o == null) o = default
        cache = cache + (uri -> o)
        o
      }
    }
  }
  
  /**
   * This is a helper function to parse String into valid
   * XHTMLPane documents.
   * @param docString A Document that must be in XML format
   * @return A valid Document or null is something failed
   */
  def parsePanelDocument(docString:String):Document = {
    val builder = dbfac.newDocumentBuilder()
    try {
      builder.parse(new java.io.ByteArrayInputStream(docString.getBytes))
    } catch {
      case e => 
        logger.debug("Failed to parse document: {}")
        logger.warn("Parsing of document failed",e)
        null
    }
  }

  def isStartupComplete() = (missingIcon != null)
  
}

class XHTMLPane extends Component {
  override lazy val peer =  new XHTMLPanel(XHTMLPane.LocalUserAgent)

  private val xpanel = peer.asInstanceOf[XHTMLPanel]

  xpanel.getSharedContext().getTextRenderer.setSmoothingThreshold(0)

  def setDocumentFromText(str:String) {
      val doc = if(str == "" || str == null) XHTMLPane.blankDocument
                else XHTMLPane.parsePanelDocument(str)
      if(doc != null) xpanel.setDocument(doc)
      else xpanel.setDocument(XHTMLPane.errorDocument)
  }
  
  def setDocument(doc:Document) {
    xpanel.setDocument(doc)
  }
}
