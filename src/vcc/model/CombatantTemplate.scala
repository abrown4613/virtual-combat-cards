//$Id$
package vcc.model

import scala.xml.{Node,NodeSeq,Elem,Null}

object XMLLoaderUtilities {
  def attribOrDefault[T](node:scala.xml.NodeSeq, default:T, conv:NodeSeq=>T):T={
    if(node.isEmpty) default
    else conv(node)
  }
  
  def nodeSeq2Int(node:NodeSeq):Int= node.text.toInt
  def nodeSeq2String(node:NodeSeq):String= node.text
}

object CombatantTemplate {
  import XMLLoaderUtilities._
  
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
    
    var defblock=(node \ "defense")
    if(!defblock.isEmpty)
      x.defense=DefenseBlock.fromXML(defblock.first)
    
    x
  }
}

/**
 * Base combatant, this can be loaded and copied if necessary.
 * @param name Name of Combatant
 * @param hp Hitpoints, mandatory
 * @param init Initiative bonus
 * @param ctype CombatantType of this combatant
 */
class CombatantTemplate(val name:String,val hp:Int, val init:Int, val ctype:CombatantType.Value) {
  import scala.xml.{UnprefixedAttribute,Null}
  
  var id:String=null
  var defense:DefenseBlock=null
  
  if(ctype==CombatantType.Minion && hp>1) throw new Exception("Minions cannot have more than 1 HP")
  
  def toXML():Node = {
    var childs:List[Node]=Nil
    
    var attrs=new UnprefixedAttribute("name",name,Null)
    attrs=new UnprefixedAttribute("init",init.toString,attrs)
    if(id!=null) attrs=new UnprefixedAttribute("id",this.id,attrs)
    if(ctype!=CombatantType.Minion) attrs=new UnprefixedAttribute("hp",hp.toString,attrs)
    
    if(defense!=null) childs= defense.toXML :: childs
    
    var elem=new Elem(null, ctype.toString.toLowerCase, attrs, scala.xml.TopScope, childs: _*)
    elem
  }
  
  def isSame(that:CombatantTemplate):Boolean = 
    (name==that.name) &&
    (id==that.id) &&
    (hp==that.hp) &&
    (ctype==that.ctype) &&
    (init==that.init) &&
    (defense==that.defense)
}


