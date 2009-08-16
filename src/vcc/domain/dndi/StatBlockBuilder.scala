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


package vcc.domain.dndi

import scala.xml.{Elem,Node,Text}

trait StatBlockDataSource {
  def extract(string:String):Option[String]
}

object StatBlockBuilder {

  abstract class Chunk {
    def render(source:StatBlockDataSource):Seq[Node]
  }
  
  object Break extends Chunk {
    def render(source:StatBlockDataSource) = Seq(<br/>)
  }
  
  abstract class PartFormatter(key:String) extends Chunk {
	def render(source:StatBlockDataSource):Seq[Node] = {
	  val v = source.extract(key)
	  if(v.isDefined) {
        Seq((<b>{key}</b>),Text(format(v.get)))
      } else Nil	}
	def format(s:String):String
  }
  
  case class PairSpacedFmt(key:String) extends PartFormatter(key) {
    def format(s:String) = s.formatted(" %s ")
  }
  
  case class PairInitFmt(key:String) extends PartFormatter(key) {
    def format(s:String) = try { s.toInt.formatted(" %+d ")} catch { case _ => s+" "}
  }
  
  case class PairFlexFmt(key:String,fmt:String) extends PartFormatter(key) {
    def format(s:String) = s.formatted(fmt)
  }

  case class TextFormat(key:String,sep:Boolean) extends Chunk {
	def render(source:StatBlockDataSource):Seq[Node] = {
	  val v = source.extract(key) 
	  if(v.isDefined) Seq(Text(v.get.formatted(if(sep) " %s " else "%s")))
      else Nil	
    }
  }
  case class BoldFormat(key:String) extends Chunk {
	def render(source:StatBlockDataSource):Seq[Node] = {
	  val v = source.extract(key) 
	  if(v.isDefined) (<b>{v.get}</b>)
      else Nil	
    }
  }
  
  case class StaticXML(xml:Node) extends Chunk {
    def render(source:StatBlockDataSource):Seq[Node] = Seq(xml)
  }
  
  case class Line(parts: Chunk*) extends Chunk {
    override def render(source:StatBlockDataSource) = {
      val s = formatChunks(source)(parts : _*)
      if(!s.isEmpty) (s ++ <br/>)
      else s
    }
  }

  case class Para(pclass:String, parts: Chunk*) extends Chunk {
    override def render(source:StatBlockDataSource) = (<p class={pclass}>{formatChunks(source)(parts : _*)}</p>)
  }

  case class Div(divClass:String, parts: Chunk*) extends Chunk {
    override def render(source:StatBlockDataSource):Seq[Node] = (<div class={divClass}>{formatChunks(source)(parts : _*)}</div>)
  }
  
  case class IfDefined(key:String, parts: Chunk*) extends Chunk {
     override def render(source:StatBlockDataSource):Seq[Node] = if(source.extract(key).isDefined) formatChunks(source)(parts: _*) else Nil
  }
  
  case class Image(src:String) extends Chunk {
    override def render(source:StatBlockDataSource):Seq[Node] = (<img src={src} />) 
  }
  
  def formatChunks(source:StatBlockDataSource)(parts:Chunk*):Seq[Node] = {
    val ns = parts.map(part => part.render(source))
    ns.flatMap(x=>x)
  }
  
}

abstract class StatBlockBuilder[T] {

  def generate(source:T):Node

}

object MonsterStatBlockBuilder { //extends StatBlockBuilder {
  import StatBlockBuilder._
  
  private val headBlock = Para("flavor",
        PairInitFmt("Initiative"),PairSpacedFmt("Senses"),Break,
          PairFlexFmt("HP"," %s; "),PairSpacedFmt("Bloodied"),Break,
          Line(PairSpacedFmt("Regeneration")),
          PairFlexFmt("AC"," %s; "),PairFlexFmt("Fortitude"," %s, "),PairFlexFmt("Reflex"," %s, "),PairSpacedFmt("Will"),Break,
          Line(PairFlexFmt("Immune"," %s; "),PairFlexFmt("Resist"," %s; "),PairFlexFmt("Vulnerable"," %s; ")),
          Line(PairInitFmt("Saving Throws")),Line(PairSpacedFmt("Speed")),Line(PairSpacedFmt("Action Points"))
      ) 
  private val tailBlock = Para("alt flavor",
        PairSpacedFmt("Alignment"),PairSpacedFmt("Languages"),Break,
          Line(PairSpacedFmt("Skills")),
          PairSpacedFmt("Str"),PairSpacedFmt("Dex"),PairSpacedFmt("Wis"),Break,
          PairSpacedFmt("Con"),PairSpacedFmt("Int"),PairSpacedFmt("Cha"),Break,
          Line(PairSpacedFmt("Equipment"))
      )
  
  class MonsterStatBlockDataSource(val monster:Monster) extends StatBlockDataSource {
    def extract(key:String):Option[String] = monster(key.toUpperCase)
  }
  
  def renderPower(power:Monster#Power): Seq[Node] = {
    List((<p class="flavor alt">{power.name}</p>),(<p class="flavorIndent">{power.desc}</p>))
  }

/*
  case class Power(icon:Parser.IconType.Value,name:String,action: String, keywords:String, desc:String) {
    var secondary:String=null
*/
  
  class PowerStatBlockDataSource(power:Monster#Power) extends StatBlockDataSource {
    def extract(key:String):Option[String] = {
      val s:String = key.toUpperCase match {
        case "NAME" => power.name
        case "DESCRIPTION" => power.desc
        case "ACTION" => power.action
        case "SECONDARY ATTACK" => power.secondary
        case "TYPE" => if(power.icon!=null) power.icon.toString else null
        case "KEYWORDS" => power.keywords
        case _ => null
      }
      if(s!=null) Some(s) else None
    }
  }
  
  object PowerFormatter extends Chunk {
    def render(mds:StatBlockDataSource):Seq[Node] = {
      println("Here")
      mds match {
        case mon: MonsterStatBlockDataSource =>
          val cl = Seq(
          	Para("flavor alt", TextFormat("Type",true), BoldFormat("Name"),TextFormat("Action",true),IfDefined("Keywords",Image("x.gif"),BoldFormat("Keywords"))),
          	Para("flavorIndent", TextFormat("Description",true)), //TODO Handle line break for beholder
          	IfDefined("Secondary Attack",Para("flavor", StaticXML(<i>Secondary Attack</i>)),Para("flavorIndent",TextFormat("Secondary Attack",true)))
           )
          val pl = for(power <- mon.monster.powers) yield {
            println(power)
            formatChunks(new PowerStatBlockDataSource(power))(cl: _*)
          }  
          val l:Seq[Node] = pl.flatMap(x=>x)
          l foreach println
          l 
        case s => Nil
      }
    }
  }
  
  def generate(monster:Monster) = {
    val mds = new MonsterStatBlockDataSource(monster)
    (<html><head><link rel="stylesheet" type="text/css" href="dndi.css" /></head>
     <body><div id="detail">
      <h1 class="monster">{monster("NAME").get}<span class="type">{monster("TYPE").get + " " + monster("ROLE").get}</span></h1>
      { formatChunks(mds)(headBlock,PowerFormatter,tailBlock)}
      </div></body></html>)
//      { monster.powers.map(renderPower).flatMap(x=>x) }
    }
}