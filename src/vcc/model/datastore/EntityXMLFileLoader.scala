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

import scala.xml.{NodeSeq,Node}

class InvalidEntityXMLException(error:String) extends Exception {
  override def getMessage()= "InvalidEntityXML: "+error
}

object EntityXMLFileLoader {

  private def extractFirstNodeText(ns:NodeSeq):String = {
    if(ns != null && ns.length > 0 ) ns(0).text
    else null
  }
  def dataFromXML(xml:scala.xml.Node):EntitySource = {
    
    def extractDatum(datum:Node,prefix:String, idx:Int):Datum = {
      val field=extractFirstNodeText(datum \ "@id")
      if(field == null) throw new InvalidEntityXMLException("Missing 'id' attribute on datum " + datum)
      new Datum(prefix,idx,field,datum.text)
    }

    if(xml.label != "entity") throw new InvalidEntityXMLException("Can't find entity")
    
    val classId= extractFirstNodeText(xml \ "@classId")
    if(classId==null) throw new InvalidEntityXMLException("Missing 'classId' attribute on entity")
    val id= extractFirstNodeText(xml \ "@id")
    if(id==null) throw new InvalidEntityXMLException("Missing 'id' attribute on entity")

    var dl:List[Datum] = Nil
    
    for(set <- xml \ "set") {
      val sid=extractFirstNodeText(set \ "@id")
      if(sid==null) throw new InvalidEntityXMLException("Missing 'id' attribute on set" + set)
      dl = dl ::: (set \ "datum").map { node=>extractDatum(node,sid,0) } .toList
    }
    
    for(mset <- xml \ "mset") {
      val msid=extractFirstNodeText(mset \ "@id")
      if(msid==null) throw new InvalidEntityXMLException("Missing 'id' attribute on set" + mset)
      for(set <- (mset \ "set")) {
    	val idx= try { extractFirstNodeText(set \ "@id").toInt } catch { case _ => throw new InvalidEntityXMLException("Missing or invalid 'id' attribute on set " + set)}
    	dl = dl ::: (set \ "datum").map {node=>extractDatum(node,msid,idx) }.toList
      }
    }

    new ListEntitySource(classId,id,dl)
  }
}
