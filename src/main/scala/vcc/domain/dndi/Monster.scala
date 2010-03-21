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
import vcc.domain.dndi.Monster.PowerDescriptionSupplement

object Monster {
  
  //Construction elements
  case class Aura(name:String,desc:String) extends StatBlockDataSource {
    def extract(key:String):Option[String] = {
      val s:String = key.toUpperCase match {
        case "NAME" => name
        case "DESCRIPTION" => desc
        case _ => null
      }
      if(s!=null) Some(s) else None
    }
    def extractGroup(dontcare:String) = Nil
  }
  
  //TODO: This is an ugly hack. Need to make this nicer.
  @deprecated
  private final val imageMap = Map(Parser.IconType.imageDirectory.map(x => (x._2,x._1)).toSeq: _*)

  case class PowerDescriptionSupplement(emphasis:String,text:String) extends StatBlockDataSource {

    def extract(key:String):Option[String] = {
      val s:String = key.toUpperCase match {
        case "HEADER" => emphasis
        case "DESCRIPTION" => text
        case _ => null
      }
      if(s!=null) Some(s) else None
    }

    def extractGroup(dontcare:String) = Nil
  
  }

  case class Power(icon:Seq[Parser.IconType.Value],name:String,action: String, keywords:String) extends StatBlockDataSource {

    private var _supplemnt:List[PowerDescriptionSupplement] = Nil
    
    def extract(key:String):Option[String] = {
      val s:String = key.toUpperCase match {
        case "NAME" => name
        case "DESCRIPTION" => description
        case "ACTION" => action
        //FIXME
        case "TYPE" if(icon!=null && !icon.isEmpty) => 
          icon.map(i => Parser.IconType.iconToImage(i)).mkString(";")
        case "KEYWORDS" => keywords
        case _ => null
      }
      if(s!=null) Some(s) else None
    }

    def extractGroup(key:String) = if(key.toUpperCase == "POWER DESCRIPTION SUPPLEMENT") this._supplemnt.toSeq else Nil

    var description: String = null

    def addDescriptionSupplement(sup:PowerDescriptionSupplement) {
      _supplemnt = _supplemnt ::: List(sup)
    }

    override def toString:String = "Power("+icon+", "+name+", "+action+", "+keywords+", "+description+", "+this._supplemnt+")"

    def supplement = _supplemnt
  }  

}

/**
 * Base monster load.
 */
class Monster(val id:Int) extends DNDIObject with StatBlockDataSource {
    
  def extract(key:String):Option[String] = this(key.toUpperCase)
  
  def extractGroup(group:String) = group.toUpperCase match {
    case "POWERS" => this.powers
    case "AURAS" => this.auras
  }
  
  import vcc.domain.dndi.Parser._
  
  private var _map=Map.empty[String,String]
  private var _auras:List[Monster.Aura]=Nil
  private var _power:List[Monster.Power]=Nil
  
  def powers = _power
  def auras = _auras

  def apply(attribute: String):Option[String] = {
    if(_map.contains(attribute)) Some(_map(attribute))
    else None
  }
  
  private [dndi] def set(attribute:String,value:String) {
    // Normalize some cases here:
    val normAttr = attribute.toUpperCase
    val normValue:String = normAttr match {
      case "HP" if(value.startsWith("1;")) => "1"
      case "ROLE" if(value == "Minion") => "No Role"
      case _ => value
    }
    _map = _map + (normAttr -> normValue)
  } 
  
  private [dndi] def addAura(name:String,desc:String):Monster.Aura = {
    val a = new Monster.Aura(name,desc)
    _auras = _auras ::: List(a)
    a
  } 
  
  protected[dndi] def addPower(icons:Seq[Parser.IconType.Value],name:String,action: String, keywords:String):Monster.Power = {
    val p = Monster.Power(icons,name,action, keywords)
    _power= _power ::: List(p)
    p
  }
  
  override def toString():String = {
    "Monster["+id+"]("+_map+"; Aura="+_auras+"; powers="+_power+")"
  }
}

import Parser._
trait BlockReader {
  
  def processBlock(block:BlockElement):Boolean
  
  def getObject:DNDIObject
}

class MonsterBuilder(monster:Monster) extends BlockReader{
  
  final val reXP= new Regex("\\s*XP\\s*(\\d+)\\s*")
  final val reLevel= new Regex("^\\s*Level\\s+(\\d+)\\s+(.*)$")
  final val reSecondary = """^Secondary Attack\s*(.*)\s*$""".r
  final val primaryStats=Set("Initiative", "Senses", "HP", "Bloodied",
                       		"AC", "Fortitude", "Reflex", "Will",
                       		"Immune", "Resist", "Vulnerable",
                       		"Saving Throws","Speed",  "Action Points")
  
  private var _lastPower:Monster.Power = null

  private var _powerSupplement:String = null
  
  def getObject:DNDIObject = monster

  /**
   * Extract xp, level and role from compose text of the header
   */
  private def normalizeTitle(l:List[(String,String)]): List[(String,String)] = {
    l match {
      case ("xp",this.reXP(xp)):: rest => ("xp",xp) :: normalizeTitle(rest)
      case ("level",this.reLevel(lvl,role)):: rest => ("level",lvl)::("role",role):: normalizeTitle(rest)
      case p :: rest => p :: normalizeTitle(rest)
      case Nil => Nil
    }
  }
  
  /**
   * Add key value pairs into the _map variable. Key will be turned into uppercase
   */
  private def addToMap(items: List[(String,String)]) {
    for((k,v)<- items) { monster.set(k,v) }
  }

  /**
   * Primary block includes auras, that become pais, need to pull them out
   */
  private def processPrimaryBlock(pairs: List[(String,String)]) {
    for( (k,v)<- pairs ) {
      if(primaryStats(k))  monster.set(k,v)
      else monster.addAura(k,v)
    }	
  }

  

  object PowerHeader {
    def unapply(parts:List[Part]):Option[(List[IconType.Value],String,String,String)] = {
	  var p = breakToSpace(parts)
	  var icons:List[IconType.Value] = Nil
      
	  while(!p.isEmpty && p.head.isInstanceOf[Icon]) {
	    icons = p.head.asInstanceOf[Icon].itype :: icons
        p = p.tail
	  }
   
	  val name:String = p match {
	    case Key(text) :: rest if(text != "Equipment") => // Need this check to avoid skipping equipments
	      text
	    case _ => return None 
	  }
	  p = p.tail
	  val action:String = p match {
	    case Text(text)::rest =>
	      p = p.tail //Advance for last part
	      text
	    case _	=> null
	  }
	  val keywords:String = p match {
	    case Icon(IconType.Separator) :: Key(text) :: Nil => text
	    case Nil => null
	    case _ => return None
	  }
      Some((icons,name,action,keywords))
    }
  }

  def processBlock(block:BlockElement):Boolean = {
    block match {
      case HeaderBlock("H1#monster",pairs) => addToMap(normalizeTitle(pairs))
      case Block("P#flavor", parts @ Key("Initiative"):: _ ) =>
        processPrimaryBlock(partsToPairs(parts))
      
      case Block("P#flavor alt", parts) =>
        parts match {
          case PowerHeader(icons,name,action,keywords) =>
            _lastPower = monster.addPower(icons,name,action,keywords)

          case Key("Equipment"):: SingleTextBreakToSpace(text) => 
          	monster.set("Equipment",text)
          
          case Key("Alignment")::rest => 
          	addToMap(partsToPairs(parts))

          case s => throw new Exception("Failed to process!! "+s)
          	return false
        }
      case Block("P#flavorIndent", SingleTextBreakToNewLine(text)) if(_lastPower != null)=> 
        // This is a power description 
        if(_powerSupplement != null) {
          _lastPower.addDescriptionSupplement(PowerDescriptionSupplement(_powerSupplement,text))
          _powerSupplement = null
        } else {
          _lastPower.description = text
        }
      case Block("P#flavor",Emphasis(comment)::Nil) if(_lastPower != null) => 
          // A secondary attack
          _powerSupplement = comment
          //_lastPower.secondaryKeywords = if(comment == "") null else comment

      case Block("P#flavor", parts @ Key("Description")::SingleTextBreakToNewLine(text)) => 
        monster.set("Description",text)
          
      case Block("P#",Emphasis(text)::Nil) => monster.set("comment",(text))
      case NonBlock(dontcare) => // Don't care
      case s =>
        throw new Exception("Failed to process: "+s)
        return false  
    }
    true
  }  
}