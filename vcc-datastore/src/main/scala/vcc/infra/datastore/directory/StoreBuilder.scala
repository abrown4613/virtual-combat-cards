/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
import java.io.File
import java.net.URI
import vcc.infra.datastore.{DataStore, DataStoreBuilder}

class StoreBuilder extends DataStoreBuilder {
  
  private final val markFileName = "store.id"
  
  private def writeMarker(dir:File) {
    val markFile = new File(dir,markFileName)
    scala.xml.XML.save(markFile.toString,(<datastore version="1.0"/>),"UTF-8",true,null)
  }
  
  private def markExists(dir:File):Boolean = {
    val markFile = new File(dir,markFileName)
    markFile.exists && markFile.canRead
  }
  
  private def readMark(dir:File):String = {
    val markFile = new File(dir,markFileName)
    try {
      val xml = scala.xml.XML.loadFile(markFile)
      (xml \ "@version")(0).text
    } catch {
      case s => null
    }
  }
  
  private def getPath(esid:DataStoreURI):File = {
    val uri = new java.net.URI(esid.getSubSchemeSpecificPart)
    val file = new File(uri)
    file
  }
  
  def open(esid:DataStoreURI):DataStore = {
    if(!exists(esid)) null
    else {
      val dir = getPath(esid)
      if(readMark(dir)=="1.0") 
        new DirectoryDataStore(dir)
      else 
        null
    }
  }
  def create(esid:DataStoreURI):DataStore = {
    if(exists(esid)) null 
    else {
      var dir = getPath(esid)
      if(!dir.exists && !dir.mkdirs) return null
      writeMarker(dir)
      new DirectoryDataStore(dir)
    }
  }
  
  def exists(esid:DataStoreURI):Boolean = {
    val dir = getPath(esid)
    if(dir.exists && dir.isDirectory) markExists(dir)
    else false
  }

  private def recursiveListFilesDirectoryFirst(f: File): Array[File] = {
    val these = f.listFiles
    these.filter(_.isDirectory).flatMap(recursiveListFilesDirectoryFirst) ++ these
  }

  def destroy(esid:DataStoreURI):Boolean = {
    val dir = getPath(esid)
    val files = recursiveListFilesDirectoryFirst(dir)
    files.foreach(_.delete())
    dir.deleteOnExit()
    !exists(esid)
  }

  def isResolvedDataStoreURI(esid:DataStoreURI):Boolean = {
    val uri = new java.net.URI(esid.getSubSchemeSpecificPart)
    !uri.isOpaque
  }
  
  /**
   * This method provides a means of expanding existant DataStoreURI that are not resolved
   */
  def resolveDataStoreURI(esid:DataStoreURI,replace:Map[String,URI]):DataStoreURI = {
    val uri = new java.net.URI(esid.getSubSchemeSpecificPart)
    val re = """^\$(\w+)\/(.*)$""".r
    if(!uri.isOpaque) return esid
    val newUri:URI = uri.getRawSchemeSpecificPart match {
      case `re`(repl,path) =>
        if(replace.isDefinedAt(repl)) replace(repl).resolve(path)
        else null
      case _ => (new File(".")).toURI.resolve(uri.getRawSchemeSpecificPart)
    }
    if(newUri != null) DataStoreURI.fromStorageString("vcc-store:directory:"+newUri.toString)
    else null
  }
}
