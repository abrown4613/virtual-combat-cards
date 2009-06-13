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

case class Datum(prefix:String,index:Int,field:String,value:String)

class UnexistantField(val classId:String, val prefix:String, val field:String) extends Exception {
  override def getMessage():String = "Unexistant field, class '"+ classId + "' does not contain field '"+prefix+":"+field+"'."
}

trait DataContainer {
  
  private[datastore] def exportData():List[Datum]
  
  def id:String
  
  def toXML:scala.xml.Node
  
  def loadDatum(datum:Datum)
}