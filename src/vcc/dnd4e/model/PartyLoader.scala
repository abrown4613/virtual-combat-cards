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

import scala.xml.{Node,NodeSeq}

object PartyLoader {
  def loadFromXML(node:Node):List[CombatantTemplate] = {
    var x:List[CombatantTemplate]=Nil
    
    if(node.label=="party") {
      for(snode <- node.child.filter(x=>x.label!="#PCDATA")) {
        try {
          x=CombatantTemplate.fromXML(snode):: x
        }catch {
          case e:Exception=>
            println("Failed to load"+snode.text)
            println("Reason "+e.toString)
        }
      }
    } else {
      println("Failed to load party")
    }
    x.reverse
  }
  
  def loadFromFile(file:java.io.File):List[CombatantTemplate] = {
    var node=scala.xml.XML.loadFile(file)
    loadFromXML(node)
  }
}
