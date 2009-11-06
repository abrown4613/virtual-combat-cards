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
  def extractGroup(group:String):Seq[StatBlockDataSource]
}

object StatBlockBuilder {

  abstract class Chunk {
    def render(source:StatBlockDataSource):Seq[Node]
    
    def formatChunks(source:StatBlockDataSource)(parts:Chunk*):Seq[Node] = parts.map(part => part.render(source)).flatMap(x=>x)
  
  }
  
  case class ChunkGroup(chunks: Chunk*) extends Chunk {
    def render(source:StatBlockDataSource):Seq[Node] = formatChunks(source)(chunks: _*)
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

  case class MultiLineFormat(key:String) extends Chunk {
	def render(source:StatBlockDataSource):Seq[Node] = {
	  def breakLines(l:List[Node],brk:Boolean):List[Node] = {
	    if(l.isEmpty) Nil else { 
		  if(brk) (<br/>) :: breakLines(l,false)
		  else l.head :: breakLines(l.tail,true)  
  	    }
      }
	  val v = source.extract(key)
	  if(v.isDefined) {
	    val lines = v.get.split("\n").map(Text(_)).toList
	    breakLines(lines,false)
	  } else Nil	
    }
  }

  case class BoldFormat(key:String,sep:Boolean) extends Chunk {
	def render(source:StatBlockDataSource):Seq[Node] = {
	  val v = source.extract(key) 
	  if(v.isDefined) (<b>{v.get.formatted(if(sep) " %s " else "%s")}</b>)
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
  
  case class ImageMap(key:String) extends Chunk {
    def render(source:StatBlockDataSource):Seq[Node] = {
      val v = source.extract(key)
      if(v.isDefined) (<img src={v.get} />)
      else Nil
    }
  }
  
  case class Group(group:String,chunks: Chunk*) extends Chunk {
    def render(source:StatBlockDataSource):Seq[Node] = {
      val gds = source.extractGroup(group)
      gds.map(subds => formatChunks(subds)(chunks: _*)).flatMap(x=>x)
    }
  }
  
}

trait StatBlockBuilder  {
  def generate(ds:StatBlockDataSource):Node
}

object MonsterStatBlockBuilder extends StatBlockBuilder {
  import StatBlockBuilder._
  
  private val headBlock = Para("flavor",
        Line(PairInitFmt("Initiative"),PairSpacedFmt("Senses")),
        Group("Auras",Line(BoldFormat("Name",true),TextFormat("description",true))),
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

  private val powerBlock = Seq(
          	Para("flavor alt", ImageMap("Type"), BoldFormat("Name",true),TextFormat("Action",true),IfDefined("Keywords",Image("x.gif"),BoldFormat("Keywords",true))),
          	Para("flavorIndent", MultiLineFormat("Description")), //TODO Handle line break for beholder
          	IfDefined("Secondary Attack",Para("flavor", StaticXML(<i>Secondary Attack</i>),TextFormat("secondary keywords",true)),Para("flavorIndent",TextFormat("Secondary Attack",true)))
           )
  
  
  def generate(ds:StatBlockDataSource) = {
    val block = new ChunkGroup(headBlock,Group("Powers",powerBlock: _*),tailBlock)
    (<html><head><link rel="stylesheet" type="text/css" href="dndi.css" /></head>
     <body><div id="detail">
      <h1 class="monster">{ds.extract("NAME").get}<span class="type">{ds.extract("TYPE").get + " " + ds.extract("ROLE").get}</span></h1>
      { block.render(ds)}
      </div></body></html>)
    }
}