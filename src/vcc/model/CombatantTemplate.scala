package vcc.model

import scala.xml.{Node,NodeSeq}

object CombatantTemplate {
  def attribOrDefault[T](node:scala.xml.NodeSeq, default:T, conv:NodeSeq=>T):T={
    if(node.isEmpty) default
    else conv(node)
  }
  
  def nodeSeq2Int(node:NodeSeq):Int= node.text.toInt
  def nodeSeq2String(node:NodeSeq):String= node.text
  
  def fromXML(node:scala.xml.Node):CombatantTemplate = {
    var name=(node \ "@name").text
    if(name=="") throw new Exception("Definintion is missing 'name' attribute")
    var hp=attribOrDefault[Int](node \ "@hp", 1,nodeSeq2Int)
    var init=attribOrDefault[Int](node \ "@init", 0,nodeSeq2Int)
    var l=CombatantType filter (x=>x.toString.toLowerCase== node.label)
    var ctype=if(l.hasNext) l.next else null
    if(ctype==null)
      throw new Exception("Combatant type '"+node.label+"' unknown")
    var x=new CombatantTemplate(name,hp,init,ctype)
    x.id=attribOrDefault(node \ "@id", null,nodeSeq2String)
    if(x.id!=null) x.id=x.id.toUpperCase
    x
  }
}

class CombatantTemplate(val name:String,val hp:Int, val init:Int, val ctype:CombatantType.Value) {
  var id:String=null
}


