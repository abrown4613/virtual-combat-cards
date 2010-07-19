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

package vcc.infra.datastore.naming

import java.util.UUID
import java.net.URI

case class EntityID(id:UUID) {
  def uri = new java.net.URI("vcc-ent:"+id)
  
  def asStorageString:String = "vcc-ent:"+id.toString
  
}

object EntityID {
 
  def fromName(name:String):EntityID = EntityID(UUID.nameUUIDFromBytes(name.getBytes))
  
  def generateRandom():EntityID = EntityID(UUID.randomUUID())
  
  def fromStorageString(s:String):EntityID = {
    if(s.startsWith("vcc-ent:")) 
      try { EntityID(UUID.fromString(s.substring(8))) } catch { case _ => null}
    else
      null
  }
}

case class DataStoreURI(uri:URI) {
  
  def asStorateString = uri.toString

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
  
  def fromStorageString(str:String):DataStoreURI =  {
    val vuri = validateURI(str,"vcc-store")
    if(vuri != null) DataStoreURI(vuri) else null
  }

}