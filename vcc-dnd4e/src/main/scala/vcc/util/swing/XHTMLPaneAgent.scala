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

import javax.swing.ImageIcon
import org.xhtmlrenderer.swing.AWTFSImage
import java.io.File
import org.xhtmlrenderer.resource.{CSSResource, ImageResource}
import vcc.infra.startup.StartupStep

/**
 * ContentCache is a simple caching mechanism used by the XHTMLPaneAgent to store images.
 * @param default If item is not found in cache and cant be loaded will returns this element
 * @param fetcher Function used to fetch a element to the cache.
 */
class ContentCache[T](default: T, fetcher: String => T) {
  private var cache = Map.empty[String, T]

  def getObject(uri: String): T = {
    if (cache.isDefinedAt(uri)) cache(uri)
    else {
      var o = fetcher(uri)
      if (o == null) o = default
      cache = cache + (uri -> o)
      o
    }
  }
}

/**
 * This is our local implementation of the Flying Saucer Agent
 * @param baseDir File representing a directory that will contain images and css files (must be in images and css
 * sub-directory.
 */
class XHTMLPaneAgent(baseDir: File) extends org.xhtmlrenderer.swing.NaiveUserAgent {
  private val logger = org.slf4j.LoggerFactory.getLogger("fs-agent")

  private val missingIcon: ImageIcon = {
    val url = this.getClass.getResource("/vcc/util/swing/missing.png")
    try {
      val icon = new javax.swing.ImageIcon(url)
      if (icon == null) vcc.infra.AbnormalEnd(this, "Failed to read MissingIcon image", null)
      icon
    } catch {
      case s => vcc.infra.AbnormalEnd(this, "Failed to read MissingIcon image", s)
    }
  }

  private val imageCache = new ContentCache[ImageResource](
    new ImageResource(AWTFSImage.createImage(missingIcon.getImage)),
    uri => {
      logger.debug("SF-UA Requested load of image: {}", uri)
      val imgName = uri.substring(uri.lastIndexOf('/') + 1).toLowerCase
      val file = new File(new File(baseDir, "images"), imgName)
      logger.debug("SF-UA Image {} mapped to {}", uri, file)
      if (file.exists && file.canRead && file.isFile) {
        try {
          new ImageResource(AWTFSImage.createImage((new ImageIcon(file.toURI.toURL)).getImage))
        } catch {
          case e =>
            logger.warn("SF-UA Failed to load file " + file, e)
            null
        }
      } else {
        logger.warn("SF-UA File {} not found or not readable", file)
        null
      }
    })


  override def getImageResource(uri: String): ImageResource = {
    imageCache.getObject(uri)
  }

  override def getCSSResource(uri: String): CSSResource = {
    logger.debug("SF-UA request CSSResource for URI: {}", uri)
    val file = new File(new File(baseDir, "css"), uri)
    logger.debug("SF-UA URI {} mapped to file {}", uri, file)
    if (file.exists && file.canRead && file.isFile) {
      logger.debug("SF-UA Reading CSS file: {}", file)
      new CSSResource(new java.io.FileInputStream(file))
    } else {
      logger.warn("SF-UA  CSS file: {} is not readable or does not exists, sending blank", file)
      new CSSResource(new java.io.ByteArrayInputStream("".getBytes))
    }
  }

  override def resolveURI(uri: String): String = {
    logger.debug("SF-UA request resolveURI: {}", uri)
    super.resolveURI(uri)
    val nuri = if (uri.startsWith("http://")) null
    else uri // Avoid relative URL that do not math this when base URL is not set
    logger.debug("SF-UA Mapped to URI: {}", nuri)
    nuri
  }
}

/**
 * Singleton to house a single Agent definition
 */
object XHTMLPaneAgent extends StartupStep {
  private var instance: XHTMLPaneAgent = null

  def isStartupComplete() = (instance != null)

  def createInstance(baseDir: File) {
    synchronized {
      instance = new XHTMLPaneAgent(baseDir)
    }
  }

  def getInstance() = instance
}