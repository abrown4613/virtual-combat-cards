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
package vcc.domain.dndi

import scala.util.matching.Regex
import scala.xml.{Node,NodeSeq,Text=>XmlText,Elem}

object Monster {
  val reXP= new Regex("\\s*XP\\s*(\\d+)\\s*")
  val reLevel= new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")
  val primaryStats=Set("Initiative", "Senses", "HP", "Bloodied",
                       "AC", "Fortitude", "Reflex", "Will",
                       "Immune", "Resist", "Vulnerable",
                       "Saving Throws","Speed",  "Action Points")
}

/**
 * Base monster load.
 */
class Monster(xml:scala.xml.Node, val id:Int) extends DNDIObject {
  import vcc.domain.dndi.Parser._
  
  //Contruction elements
  case class Aura(name:String,desc:String)
  case class Power(icon:Parser.IconType.Value,name:String,action: String, keywords:String, desc:String) {
    var secondary:String=null
    
    override def toString:String = "Power("+icon+", "+name+", "+action+", "+keywords+", "+desc+", "+secondary+")" 
  }
  
  private var _map=Map.empty[String,String]
  private var _auras:List[Aura]=Nil
  private var _power:List[Power]=Nil
  
  def powers = _power.reverse
  
  def auras = _auras.reverse
  
  load(xml)
  
  /**
   * Extract text form spans, this is a in depth search and the output
   * will be pairs of the SPAN class and the first Text entry.
   */
  private def flattenSpan(xml:scala.xml.NodeSeq):List[(String,String)] = {
    var l:List[(String,String)]=Nil 
    
    l = (for(span<- xml \\ "SPAN") yield {
      ( (span \ "@class").first.toString, span.first.child(0).toString.trim)
    }).toList
    ("name",(xml\"#PCDATA").toString.trim):: l
  }
  
  /**
   * Extract xp, level and role from compose text of the header
   */
  private def normalizeTitle(l:List[(String,String)]): List[(String,String)] = {
    l match {
      case ("xp",Monster.reXP(xp)):: rest => ("xp",xp) :: normalizeTitle(rest)
      case ("level",Monster.reLevel(lvl,role)):: rest => ("level",lvl)::("role",role):: normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }

  /**
   * Replace Break() in descriptin for \n and merge text. This should create a large block
   */
  private def formatDescription(parts:List[Part]) = {
    val t=Parser.mergeText(parts.map(x=> if(x==Break()) Text("\n") else x))
    t match {
      case Text(text) :: Nil => text
      case _ => throw new Exception("Description should be a single text block "+t)
    }
  }
  
  /**
   * Primary block includes auras, that become pais, need to pull them out
   */
  private def processPrimaryBlock(pairs: List[(String,String)]) {
    for( (k,v)<- pairs ) {
      if(Monster.primaryStats(k))  _map = _map + (k.toUpperCase->v)
      else _auras=Aura(k,v)::_auras
    }
  }
  
  /**
   * Add power to the list of powers
   */
  protected def processPower(icon:Parser.IconType.Value,name:String,action: String, keywords:String, desc:String) {
    //println((if(icon!=null) "["+icon+"] " else "")+name+" "+(if(action!=null)action else "")+(if(keywords!=null)" * "+keywords else ""))
    //println("   "+desc)
    _power=Power(icon,name,action, keywords, desc) :: _power
  }
  
  /**
   * Descriptions of powers come on the following block, this is used to extract them
   */
  private def extractDescriptionFromBlocks(blocks:List[List[Part]]):String = {
    assert(blocks.tail != Nil)
    blocks.tail.head match {
      case Text(desc):: Nil => desc
      case _ => throw new Exception("Cant find description block")
    }
  }
 
  /**
   * Add key value pairs into the _map variable. Key will be turned into uppercase
   */
  private def addToMap(items: List[(String,String)]) {
    for((k,v)<- items) { _map = _map + (k.toUpperCase -> v)}
  }
  
  /**
   * This is the main load function, that is called for the construction of this object
   */
  private def load(xml:scala.xml.Node) {
    val head=normalizeTitle(flattenSpan((xml \\ "H1").first))
    addToMap(head)

    var blocks=(xml \ "P").filter(node=> !(node \ "@class" isEmpty)).map(block=>parse(block.child)).toList
    while(blocks!=Nil) {
      //println("Block\n\t"+blocks.head)
      blocks.head match {
        case Key("Initiative") :: rest => processPrimaryBlock(partsToPairs(blocks.head))

        case Key("Description")::rest => 
          // This case is needed because of Break in the description
          _map = _map + ("DESCRIPTION" -> formatDescription(rest))
          
        case Key("Equipment")::rest => 
          // This case is needed because of Break in the description
          _map = _map + ("EQUIPMENT" -> formatDescription(rest))
        
        case Icon(icon)::Key(powername)::Text(action)::Nil =>
          // Power without keywords (like pseudo dragon fly by)
          processPower(icon,powername,action,null,extractDescriptionFromBlocks(blocks))
          blocks=blocks.tail

        case Key(powername)::Text(action)::Nil =>
          // Power without keyword or Icon (like Goblin Tactics)
          processPower(null,powername,action,null,extractDescriptionFromBlocks(blocks))
          blocks=blocks.tail
          
        case Emphasis("Secondary Attack")::Nil =>
          // A secondary attack
          assert(_power.head != Nil && _power.head.secondary==null)
          _power.head.secondary=extractDescriptionFromBlocks(blocks)
          blocks=blocks.tail
          
        case Icon(icon)::Key(powername)::Text(action)::Icon(IconType.Separator)::Key(keywords)::Nil =>
          processPower(icon,powername,action,keywords,extractDescriptionFromBlocks(blocks))
          blocks=blocks.tail
          
        case Key(powername)::Text(action)::Icon(IconType.Separator)::Key(keywords)::Nil =>
          processPower(null,powername,action,keywords,extractDescriptionFromBlocks(blocks))
          blocks=blocks.tail

        case Key(feature)::Nil => 
          processPower(null,feature,null,null,extractDescriptionFromBlocks(blocks))
          blocks=blocks.tail
          
        case Key("Alignment")::rest => 
          // This case is needed because of Break in the description
          var pairs=partsToPairs(blocks.head)
          addToMap(pairs)
          
        case s => throw new Exception("Shouldn't get here! What to do with: "+s)
      }
      blocks=blocks.tail
    }
    // Fix minion HP and Role
    if(_map.contains("HP")&& _map("HP").startsWith("1;")) _map = _map + ("HP" ->"1")
    if(_map.contains("ROLE")&& _map("ROLE") == "Minion") _map = _map + ("ROLE" ->"No Role")
  }
  
  def apply(attribute: String):Option[String] = {
    if(_map.contains(attribute)) Some(_map(attribute))
    else None
  }
  
  override def toString():String = {
    "Monster["+id+"]("+_map+"; Aura="+_auras+"; powers="+_power+")"
  }
}
