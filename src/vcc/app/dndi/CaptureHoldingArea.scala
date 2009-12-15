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

import vcc.domain.dndi.{Monster,DNDInsiderCapture}
import vcc.infra.diskcache._
import scala.xml.Node
import java.io.{File,FileInputStream}


/**
 * Object designed to hold objects and creatures captured form D&D Insider
 * and serve as an access point to both application and User interface
 */
object CaptureHoldingArea {
  
  val logger = org.slf4j.LoggerFactory.getLogger("app")
  
  trait CaptureHoldingObserver[T] {
	def updateContent(objects:Seq[T])
  }

  object XMLLoader extends DiskCacheBuilder[MonsterCacheEntry] {
    def loadFromFile(file:java.io.File):MonsterCacheEntry = {
      try {
        val rawXML = DNDInsiderCapture.pluginInputStreamAsFilteredString(new FileInputStream(file))
        val xml = scala.xml.XML.loadString(rawXML)
        val dndiObject = DNDInsiderCapture.load(xml)
        dndiObject match {
          case monster: Monster => new MonsterCacheEntry(monster,xml)
          case _ => null
        }
      } catch {
        case e => 
          logger.warn("Failed to load file {} reason: {}",file,e.getMessage)
          null
      }
    }
  }
  
  class MonsterCacheEntry(val monster:Monster, val node:Node) extends DiskCacheable {
    def saveToCache(file:java.io.File):Boolean = {
      scala.xml.XML.save(file.getAbsolutePath,node,"UTF-8")
      file.exists && file.isFile && file.canRead
    }
    override def getCacheFileName():String = {
      val id = if(node.attribute("id").isDefined) node.attribute("id").get else super.getCacheFileName()
      "monster-" + id + ".xml"
    }
  } 
  
  private var monsters:List[Monster] = Nil
  private var monstersObserver:List[CaptureHoldingObserver[Monster]] = Nil
  
  private val diskCache = new DiskCache[MonsterCacheEntry]({
	  val base = new java.io.File(vcc.dnd4e.Configuration.baseDirectory.value,"dndicache/monster")
	  if(!base.exists) base.mkdirs()
      base
    },XMLLoader)
  
  def addCapturedMonsterAndCacheXML(monster:Monster, xml:scala.xml.Node) {
    diskCache.save(new MonsterCacheEntry(monster, xml))
    monsters = monster :: monsters
    notifyObservers()
  }
  
  def loadCachedMonster() {
    monsters = diskCache.loadAll().map(me => me.monster).toList
    notifyObservers()
  }
  
  def clearCachedMonster() {
    diskCache.clear()
  }
  
  def addMonsterObserver(obs: CaptureHoldingObserver[Monster]) {
    monstersObserver = obs :: monstersObserver
  }
  
  protected def notifyObservers() {
    monstersObserver.foreach {obs => obs.updateContent(monsters)}
  }
}

object CaptureStore {
  import java.io.File
  //import scala.xml.Node
  val logger = org.slf4j.LoggerFactory.getLogger("app")

  def storeObject(otype:String, oid:Int, baseDir:File, node:Node):Boolean = {
    val targetDir = new File(baseDir,"/dndicache/"+otype)
    if(!targetDir.exists) targetDir.mkdirs()
    if(targetDir.exists) {
      val file = new File(targetDir,otype+"-"+oid+".xml")
      scala.xml.XML.save(file.getAbsolutePath,node,"UTF-8")
      logger.debug("Saved {} with id={} to: {}",Seq(otype,oid.toString,file.toString).toArray)
      file.exists && file.isFile && file.canRead
    } else false
  }
}