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
//$Id$
package vcc.model.datastore

trait EntitySource {
  def getData():Iterator[Datum]
  def classId:String
  def id:String
}

class ListEntitySource(val classId:String,val id:String, lst:Seq[Datum]) extends EntitySource {
  def getData() = lst.elements
}

trait EntityDestination

object EntityLoader {
  
  def load(source:EntitySource):Entity = {
    if(!EntityFactory.isClassDefined(source.classId)) 
      throw new Exception("Cant find classId="+source.classId)
	val entity = EntityFactory.createInstance(source.classId,source.id)
    for(datum <- source.getData()) {
      entity.loadDatum(datum)
    }
    entity
  }
  
  def load():Entity = {
    /*
     * High level logic:
     * 
     * 1) Look on EntityIndex for this object will get A Datasouce
     * 2) Load From datasource
     * 
     */
    null
  }
  
  def store(ent:Entity) {
    /*
     

     */
  }
}
