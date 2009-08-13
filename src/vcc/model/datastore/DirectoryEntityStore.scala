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

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")
  
  private var _map = Map.empty[EntityID,Entry]
  
  private val baseDir = DirectoryEntityStore.extractBaseDirectory(esid) 
  if(baseDir == null) throw new EntityStoreException("Base directory not defined correctly")
  private val marker = new DirectoryEntityStore.RepositoryMark(DirectoryEntityStore.getMarkerFile(baseDir))
  if(!marker.exists) throw new EntityStoreException("Cant find generate marker file, maybe not a real DirectoryEntityStore "+marker)

  private val indexFile = new File(baseDir,"index.toc")
  scanIndex()  
  
  private def scanIndex() {
    logger.info("Scaning index file {}",indexFile)
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
	      logger.error("Bad line: {}", line)
	    }
	    line = is.readLine()
	  }
	  is.close()
	} else {
	  reIndex()
	}
  }
  
  private def reIndex() {
    logger.info("Reindexing directory {}",baseDir)
    indexFile.delete()
	val os = new java.io.PrintWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(indexFile,true)))
    var flist = new vcc.util.DirectoryIterator(baseDir,true).filter( x => x.toString.endsWith(".xml"))
    for(file <- flist) {
      logger.debug("Parsing file {}",file)
      val ent = loadFile(file)
      if(ent != null) {
        _map = _map + (ent.id-> new Entry(ent.classId,file,ent))
        os.println(List(ent.id.uri.toString,ent.classId.uri.toString,file.getName).mkString("|"))
      }
    }
	os.close()
  }
  
  private def saveIndexEntry(eid:EntityID, classId:EntityClassID, file:File) {
	val os = new java.io.PrintWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(indexFile,true)))
	os.println(List(eid.uri.toString,classId.uri.toString,file.getName).mkString("|"))
	os.close()
  }
  
  private def loadFile(file:File):Entity = {
    try {
      val xml = scala.xml.XML.loadFile(file)
      val ds=EntityXMLFileLoader.dataFromXML(xml)
      if(ds != null)
        EntityFactory.createInstance(ds)
      else null
    } catch {
      case e => 
        logger.error("Failed to load file: "+file,e)
        null
    }
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
    } else {
      // Clear map or we run into trouble
      _map = _map - eid
      null.asInstanceOf[Entity]
    } 
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

  def nextSequential() = marker.getNextInSequence()
}

object DirectoryEntityStore extends EntityStoreBuilder {

    
  private class RepositoryMark(markFile:File) {

    if(markFile == null) throw new Exception("Invalid markerFile")
    private var seq = 0
    if(markFile.exists) load()
    
    def exists() = markFile.exists() 
    
    def getNextInSequence():Int = {
       seq += 1
       save()
       seq
    }
    
    def save() {
      scala.xml.XML.saveFull(markFile.toString,
        <repository version="1.0"><sequence id="base" value={seq.toString} /></repository>
      , "UTF-8",true,null)
    }
    
    def load() {
      try {
    	val xml = scala.xml.XML.load(new java.io.FileInputStream(markFile))
    	val s = (xml \\ "sequence")(0)
    	seq = s.attribute("value").get.text.toInt
      } catch {
        case e => throw new Exception("Failed load marker file",e)
      }
    }
  }

  
  /**
   * Destroy and eliminate all data in the repository.
   */
  def destroy(esid:EntityStoreID) {
    val dir = extractBaseDirectory(esid)
    if(dir != null) new vcc.util.DirectoryIterator(dir,false) foreach (x => x.delete)
  }
  
  /**
   * Create a new instance of the repository. In this step all necessary steps to create
   * the repository. It should also open the repository.
   */
  def create(esid:EntityStoreID):EntityStore = {
    val baseDir = extractBaseDirectory(esid)
    val markFile = getMarkerFile(baseDir)
    if(!exists(esid)) {
    	if(!baseDir.exists) baseDir.mkdirs
    	val rm = new RepositoryMark(markFile)
    	rm.save()
    	open(esid) 
    } else throw new EntityStoreException("Cannot create "+esid+" since it already exists")
  }
  
  /**
   * Open an existing respository. If the repository has not been create an exceptions
   * will be thrown.
   */
  def open(esid:EntityStoreID):EntityStore = {
    if(isValidEntityStoreID(esid)) {
      if(!exists(esid)) throw new EntityStoreException("Cant open "+esid+" since it has not been created")
      val es = new DirectoryEntityStore(esid)
      assert(es!=null)
      es
    } else null
  }
  
  /**
   * Test if the repository is already created.
   * @return False if the repository does not exist
   */
  def exists(esid:EntityStoreID):Boolean = {
    val baseDir = extractBaseDirectory(esid)
    val markFile = getMarkerFile(baseDir)
    (baseDir != null && markFile != null && baseDir.exists && baseDir.isDirectory && markFile.exists && markFile.isFile) 
  }
  
  /**
   * Test if the EntityStoreID is a valid ID for this type of entity store.
   * @return False if the repository does not exist
   */
  def isValidEntityStoreID(esid:EntityStoreID):Boolean = (esid.subScheme == "directory") && (extractBaseDirectory(esid) != null)
  
  protected def extractBaseDirectory(esid:EntityStoreID):File = {
    try {
      if(esid.subScheme == "directory") {
        val dir = new File(new java.net.URI(esid.getSubSchemeSpecificPart))
        if( (dir.exists && dir.isDirectory) || ( ! dir.exists) ) dir
        else null
      } else null
    } catch { case _ => null }
  }
  
  protected def getMarkerFile(baseDir:File):File = 
    if(baseDir != null) new File(baseDir,"repository.dat")
    else null
  
}
