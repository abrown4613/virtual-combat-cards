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
