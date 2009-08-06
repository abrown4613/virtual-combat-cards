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
package vcc.model.datastore

import scala.reflect.Manifest
import java.io.File

class DirectoryEntityStore(esid:EntityStoreID) extends EntityStore {
  
  private class Entry(val classId:EntityClassID, val file:File, var entity:Entity) 
  
  private var _map = Map.empty[EntityID,Entry]
  
  private val baseDir = try {
    if(esid.subScheme == "directory") {
      new File(new java.net.URI(esid.getSubSchemeSpecificPart))
    } else null
  } catch { case _ => null }
  
  assert(baseDir != null)
  if(!baseDir.exists) baseDir.mkdirs()
  assert(baseDir.isDirectory)
  
  private val indexFile = new File(baseDir,"index.toc")
  scanIndex()  
  
  private def scanIndex() {
	if(indexFile.exists) {
	  val is = new java.io.BufferedReader(new java.io.InputStreamReader(new java.io.FileInputStream(indexFile)))
	  var line = is.readLine()
	  while(line != null) {
	    val fields = line.split("\\|")
	    if(fields.length == 3) {
	      val eid = DataStoreURI.asEntityID(fields(0))
	      val classId = DataStoreURI.asEntityClassID(fields(1))
          val file = new File(baseDir,fields(2))
          if(file.exists) {
            _map = _map + (eid -> new Entry(classId,file,null))
          }
	    } else {
	      println("Bad line: " + line)
	    }
	    line = is.readLine()
	  }
	  is.close()
	} else {
	  reIndex()
	}
  }
  
  private def reIndex() {
    indexFile.delete()
	val os = new java.io.PrintWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(indexFile,true)))
    var flist = new vcc.util.DirectoryIterator(baseDir,true).filter( x => x.toString.endsWith(".xml"))
    for(file <- flist) {
      val ent = loadFile(file)
      _map = _map + (ent.id-> new Entry(ent.classId,file,ent))
      os.println(List(ent.id.uri.toString,ent.classId.uri.toString,file.getName).mkString("|"))
    }
	os.close()
  }
  
  private def saveIndexEntry(eid:EntityID, classId:EntityClassID, file:File) {
	val os = new java.io.PrintWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(indexFile,true)))
	os.println(List(eid.uri.toString,classId.uri.toString,file.getName).mkString("|"))
	os.close()
  }
  
  private def loadFile(file:File):Entity = {
    val xml = scala.xml.XML.loadFile(file)
    val ds=EntityXMLFileLoader.dataFromXML(xml)
    if(ds != null)
      EntityFactory.createInstance(ds)
    else null
  } 
  
  /**
   * Store the entity to the store. Entity must be a well formed 
   * entity, since no validation will be done.
   */
  def store(entity: Entity) {
    val xml = entity.toXML
    val file = new File(baseDir, entity.id.uri.getSchemeSpecificPart.replace(":","-") + ".xml")
    _map = _map + (entity.id -> new Entry(entity.classId,file,null))
    scala.xml.XML.save(file.toString,xml)
    saveIndexEntry(entity.id,entity.classId,file)
  }
  
  /**
   * Load entity from store, the type of the desired entity must be 
   * specified.
   */
  def load(eid:EntityID):Entity = {
    if(_map.isDefinedAt(eid)) {
      val entry = _map(eid)
      if(entry.entity == null) entry.entity = loadFile(entry.file)
      entry.entity
    } else
      null.asInstanceOf[Entity]
  }
  
  protected def getEntitySummaryMap(eid:EntityID):Map[DatumKey,String] = {
    val entity = load(eid)
    if(entity != null) {
      val meta = EntityFactory.getEntityBuilder(entity.classId)
      Map(meta.summaryFields.map(f => (f,entity.getFieldFromDatumKey(f).toStorageString)): _*)
    } else 
      Map.empty[DatumKey,String]
  }
  
  /**
   * Determine if entity exists and returns it type
   */
  def getEntityClassID(eid:EntityID):Option[EntityClassID] = {
    if(_map.isDefinedAt(eid)) {
      Some(_map(eid).classId)
    } else None
  } 

  /**
   * Delete an object
   */
  def delete(eid:EntityID) {
    if(_map.isDefinedAt(eid)) {
      _map(eid).file.delete()
      _map = _map - eid
    }
  }
  
  /**
   * Enumerate Entities of a give class.
   * @param classId If null will return all the classes
   */
  def enumerate(classId: EntityClassID):scala.collection.Set[EntityID] = {
    if(classId == null) _map.keySet
    else Set() ++ _map.map(p => if(p._2.classId == classId) p._1 else null).filter( x => x != null)
  }

}
