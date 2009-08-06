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
package vcc.dnd4e.model

import scala.xml._

import vcc.model.datastore.{EntityID,EntityStore,EntityStoreID}
import vcc.model.Registry

case class PartyMember(id: Symbol, alias:String, eid: EntityID) {
  def toXML() = <combatant eid={eid.uri.toString} alias={if(alias!=null) alias else null} id={if(id!=null) id.name else null} />
}

/**
 * Load a Party of PEML file.
 */
object PartyLoader {
  import vcc.util.XMLHelper._
  def parseEntry(store: EntityStore, node:Node):List[PartyMember] = {
    val eidUri = nodeSeq2String(node \ "@eid")
    val id = nodeSeq2String(node \ "@id", null)
    val alias = nodeSeq2String(node \ "@alias", null)
    val count = nodeSeq2Int(node \ "@count", 1)
    val eid = EntityID(new java.net.URI(eidUri))
    
    val ent=PartyMember(if(id!=null) Symbol(id.toUpperCase) else null ,alias,eid)
    // Unroll count
    (1 to count).map(x => ent).toList
  }
  
  def loadFromXML(store: EntityStoreID, node:Node):List[PartyMember] = {
    var x:List[PartyMember]=Nil
    val repository = Registry.get[EntityStore](store).get
    
    if(node.label=="party") {
      for(snode <- node.child if(snode.label!="#PCDATA")) {
        try {
          x= x ::: parseEntry(repository,snode)
        }catch {
          case e:Exception=>
            println("Failed to load node: "+snode)
            println("Reason "+e.toString)
        }
      }
    } else {
      println("Failed to load party")
    }
    x
  }
  
  def loadFromFile(store: EntityStoreID, file:java.io.File):List[PartyMember] = {
    try {
      var node=scala.xml.XML.loadFile(file)
      loadFromXML(store,node)
    } catch {
      case e =>
        println("Failed to load: "+e.getMessage)
        Nil
    }
  }
  
  def saveToFile(store:EntityStoreID, file:java.io.File,entries:Seq[PartyMember]) {
    val doc = new Elem(null,"party",new UnprefixedAttribute("esid",(if(store!=null) store.uri.toString else null),Null),TopScope,entries.map(_.toXML): _*)
    XML.saveFull(file.toString,doc,"UTF-8",true,null)
  }
  
  def loadToBattle(esid:EntityStoreID, members:Seq[PartyMember]) {
    import vcc.controller.actions.{StartTransaction,EndTransaction}
    import vcc.dnd4e.controller.request.AddCombatant
	val tracker = Registry.get[scala.actors.Actor]("tracker").get
    tracker ! StartTransaction("Party Load")
    for(x<-members) tracker ! AddCombatant(esid,x)
    tracker ! EndTransaction("Party Load")
  }
}
