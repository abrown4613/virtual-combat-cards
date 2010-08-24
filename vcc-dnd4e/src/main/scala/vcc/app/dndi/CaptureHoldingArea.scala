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

package vcc.app.dndi

import vcc.infra.diskcache._
import java.io.{File, FileInputStream}
import vcc.domain.dndi.{DNDIObject, DNDInsiderCapture}
import xml.{XML, Node}
import java.lang.String
import collection.immutable.Map

/**
 * Object designed to hold objects and creatures captured form D&D Insider
 * and serve as an access point to both application and User interface
 */
object CaptureHoldingArea {
  val logger = org.slf4j.LoggerFactory.getLogger("app")

  trait CaptureHoldingObserver[T] {
    //TODO Change this to new sequence and an options new entry
    //FIXME New entry is the hook to allow automated updates
    def updateContent(objects: Seq[T])
  }

  object XMLLoader extends DiskCacheBuilder[DNDIObjectCacheEntry] {
    def loadFromFile(file: File): DNDIObjectCacheEntry = {
      try {
        val rawXML = DNDInsiderCapture.pluginInputStreamAsFilteredString(new FileInputStream(file))
        val xml = scala.xml.XML.loadString(rawXML)
        val dndiObject = DNDInsiderCapture.load(xml)

        if(dndiObject != null) new DNDIObjectCacheEntry(dndiObject, xml)
        else null
      } catch {
        case e =>
          logger.warn("Failed to load file {} reason: {}", file, e.getMessage)
          null
      }
    }
  }

  class DNDIObjectCacheEntry(val dndiObj: DNDIObject, val node: Node) extends DiskCacheable {
    def saveToCache(file: File): Boolean = {
      XML.save(file.getAbsolutePath, node, "UTF-8")
      file.exists && file.isFile && file.canRead
    }

    override def getCacheFileName(): String = {
      val id = dndiObj.id.toString
      val clazz = dndiObj.clazz

      clazz + "/" + clazz + "-" + id + ".xml"
    }
  }

  private var entries: List[DNDIObject] = Nil
  private var observers: List[CaptureHoldingObserver[DNDIObject]] = Nil

  private val diskCache = new DiskCache[DNDIObjectCacheEntry]({
    val base = new File(vcc.dnd4e.Configuration.baseDirectory.value, "dndicache")
    if (!base.exists) base.mkdirs()
    base
  }, XMLLoader)

  /**
   * Add an entry to the holding are and notify observers.
   * @para obj The object to be stored
   * @param xml The XML source of the entry
   */
  def addCapturedEntry(obj: DNDIObject, xml: scala.xml.Node) {
    diskCache.save(new DNDIObjectCacheEntry(obj, xml))
    entries = obj :: entries
    notifyObservers()
  }

  def loadCachedEntries() {
    entries = diskCache.loadAll().map(me => me.dndiObj).toList
    notifyObservers()
  }

  def clearCachedMonster() {
    diskCache.clear()
  }

  def addObserver(obs: CaptureHoldingObserver[DNDIObject]) {
    observers = obs :: observers
  }

  protected def notifyObservers() {
    observers.foreach {obs => obs.updateContent(entries)}
  }

  /**
   * This method will store an incomplete entry. This is used for capturing data for future analysis.
   * @para clazz Class of DNDIObject
   * @para id  Class id
   * @para node XML representation of the object
   * @return True if saved successfully.
   */
  def storeIncompleteObject(clazz: String, id: Int, node: Node): Boolean = {
    class PartialDNDIObject(val clazz: String, val id: Int) extends DNDIObject {
      protected var attributes = Map.empty[String, String]
    }
    val entry = new DNDIObjectCacheEntry(new PartialDNDIObject(clazz, id), node)
    diskCache.save(entry)
  }
}
