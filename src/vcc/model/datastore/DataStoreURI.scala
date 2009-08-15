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

import java.net.URI

case class EntityID(uri: java.net.URI)

case class EntityClassID(uri: java.net.URI)

case class EntityStoreID(uri: java.net.URI) {
  private val subURI = new URI(uri.getRawSchemeSpecificPart)
  def subScheme = subURI.getScheme
  def getSubSchemeSpecificPart = subURI.getRawSchemeSpecificPart
  
  override def toString():String = uri.toString
}

object DataStoreURI {
  
  def validateURI(uri:String,scheme:String):java.net.URI = {
	try {
	  val u = new java.net.URI(uri)
	  if(u.isOpaque() && u.getScheme() == scheme) {
	    u
	  } else {
	    null
	  }
	} catch {
	  case _ => null
	}
  }
  
  def asEntityStoreID(uri:String):EntityStoreID = {
    val vuri = validateURI(uri,"vcc-store")
    if(vuri != null) EntityStoreID(vuri) else null
  }
  
  def asEntityID(uri:String):EntityID = {
    val vuri = validateURI(uri,"vcc-ent")
    if(vuri != null) EntityID(vuri) else null
  }
  
  def asEntityClassID(uri:String):EntityClassID = {
    val vuri = validateURI(uri,"vcc-class")
    if(vuri != null) EntityClassID(vuri) else null
  }
  
  def directoryEntityStoreIDFromFile(file:java.io.File):EntityStoreID = {
    asEntityStoreID("vcc-store:directory:"+file.toURI.toString)
  }
}



