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
package vcc.infra.datastore.directory

import vcc.infra.datastore.naming._
import java.io._
import vcc.infra.datastore.{DataStoreEntity}
import vcc.util.DirectoryIterator


class DirectoryDataStore(baseDir:File) extends DataStore {
  import org.xml.sax.helpers.DefaultHandler
  import org.xml.sax.Attributes
  import javax.xml.parsers.SAXParserFactory
  
  val saxFactory = SAXParserFactory.newInstance
  class EntityParser(expectedEntityID:EntityID) extends org.xml.sax.helpers.DefaultHandler {
    private var eid:EntityID = null
    private var data = Map.empty[String,String]
    
    private var currentText:StringBuilder = null
    private var currentDatum:String = null
    
	override def startElement(uri:String,localName:String,qName:String,attr:Attributes) {
	  qName match { 
	    case "entity" =>
	      if(attr.getValue("id") == null) throw new DataStoreIOException("Entity must contain entity ID",null)
	      eid = EntityID.fromStorageString(attr.getValue("id"))
	      if(expectedEntityID != eid) throw new DataStoreIOException("EntityID mistmatch",null)
	    case "datum" =>
	      currentDatum = attr.getValue("id")
	      if(currentDatum == null) throw new DataStoreIOException("Datum must contain id attribute",null)
	      else currentText = new StringBuilder()
	    case other =>
	      throw new DataStoreIOException("Unexpected xml element '" + other + "'",null)
	  }
	}
  	override def endElement(uri:String,localName:String,qName:String) {
  	  if(currentText != null && currentDatum != null) {
  	    data = data + (currentDatum -> currentText.toString)
      }
      currentText = null
      currentDatum = null
  	}
	override def characters(chars:Array[Char],start:Int,end:Int) {
	  if(currentText != null) currentText.append(chars,start,end)
	}
 
	def loadedData() = (eid,data)
  }

  private def loadXMLFile(eid:EntityID) = {
    val file = getFile(eid)
    val ep = new EntityParser(eid)
    val sp = saxFactory.newSAXParser()
    try {
      sp.parse(file,ep)
      ep.loadedData
    } catch {
      case dsioe:DataStoreIOException => throw dsioe
      case e => throw new DataStoreIOException("Failed to parse XML file",e)
    }
  }

  private def getFile(eid:EntityID) = new File(baseDir,eid.id.toString+".dsxml")
  
  def loadEntity(eid:EntityID):DataStoreEntity = {
    if(entityExists(eid)) {
      val (entid,data) = loadXMLFile(eid)
      new DataStoreEntity(entid,Map(data.map(x=>(x._1->x._2)).toSeq: _*))
    } else null
  }
   
  def storeEntity(ent:DataStoreEntity):Boolean = {
    val sortedKeys = scala.util.Sorting.stableSort[String](ent.data.keys.toList,(a:String,b:String) => a < b)
    val file = getFile(ent.eid)
    val os:java.io.PrintWriter = new PrintWriter(new FileWriter(file))
    os.println("<?xml version='1.0' encoding='UTF-8' ?>")
    os.println("<entity id='"+ent.eid.asStorageString +"'>")
    for(key <- sortedKeys) 
      os.println("  "+(<datum id={key}>{ent.data(key)}</datum>))
    os.println("</entity>")
    os.close()
    true
  }
   
  def entityTimestamp(eid:EntityID):Long = getFile(eid).lastModified
   
  def enumerateEntities():Seq[EntityID] = {
    val diter = new DirectoryIterator(baseDir,false)
    diter.map {
      file => 
        if(file.getName.endsWith(".dsxml")) EntityID.fromStorageString("vcc-ent:"+file.getName.substring(0,file.getName.length-6))
        else null.asInstanceOf[EntityID]
    }.filter(x=> x != null).toList
  }
   
  def extractEntityData(keys:Set[String]):Seq[(EntityID,Map[String,String])] = {
    enumerateEntities.map(eid => try { loadXMLFile(eid) } catch { case _ => null}).filter(x => x != null).map(ent=> {
      val (eid,data) = ent
      val fields = Map((for((id,value) <- data if(keys.contains(id))) yield (id,value)).toSeq: _*)
      (eid,fields)
    })
  }
   
  def entityExists(eid:EntityID):Boolean = getFile(eid).exists
   
  def deleteEntity(eid:EntityID):Boolean = getFile(eid).delete
   
  def close() {
    
  }

}
