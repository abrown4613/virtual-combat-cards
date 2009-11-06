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

package vcc.dnd4e.view.compendium

import vcc.util.swing.forms._
import vcc.domain.dndi.StatBlockDataSource
import vcc.domain.dndi.StatBlockBuilder

object SimpleStatBlockBuilder extends StatBlockBuilder {
  import vcc.domain.dndi.StatBlockBuilder._
  private val headBlock = Para("flavor",
    Line(
      PairInitFmt("Initiative"),PairSpacedFmt("Senses")),
      PairFlexFmt("HP"," %s; "),Break,
      PairFlexFmt("AC"," %s; "),PairFlexFmt("Fortitude"," %s, "),PairFlexFmt("Reflex"," %s, "),PairSpacedFmt("Will")
     ) 
  private val tailBlock = Para("alt flavor",
      Line(PairSpacedFmt("Insight")),
      Line(PairSpacedFmt("Perception"))
    )

  def generate(ds:StatBlockDataSource) = {
    val block = new ChunkGroup(headBlock,tailBlock,Para(null,PairFlexFmt("Comment",": %s")))
    (<html><head><link rel="stylesheet" type="text/css" href="dndi.css" /></head>
     <body><div id="detail">
      <h1 class={if(ds.extract("PCLASS").isDefined) "player" else "monster"  }>{ds.extract("NAME").get}<span class="type">{ds.extract("TYPE").get + " " + ds.extract("ROLE").get}</span></h1>
      { block.render(ds)}
      </div></body></html>)
  }  
}

class FormFieldStatBlockSource(f:Form) extends StatBlockDataSource {
  val values = f.extractMap
  
  private def wrapExtract(key:String):Option[String] = {
    if(values.isDefinedAt(key)) {
      if(values(key) == null) None
      else Some(values(key))
    } else None 
  }
  
  def extract(key:String):Option[String] = key.toUpperCase match {
    case "NAME" => wrapExtract("base:name")
    case "RACE" => wrapExtract("base:race")
    case "PCLASS" => wrapExtract("base:class")
    case "MROLE" => wrapExtract("base:role")
    case "SENSES" => wrapExtract("base:senses")
    case "HP" => wrapExtract("stat:hp")
    case "INITIATIVE" => wrapExtract("stat:initiative")
    case "AC" => wrapExtract("stat:ac")
    case "WILL" => wrapExtract("stat:will")
    case "REFLEX" => wrapExtract("stat:reflex")
    case "FORTITUDE" => wrapExtract("stat:fortitude")
    case "INSIGHT" => wrapExtract("skill:insight")
    case "PERCEPTION" => wrapExtract("skill:perception")
    case "TYPE" => if(values.isDefinedAt("base:class")) extract("RACE") else extract("ISMINION")
    case "ROLE" => if(values.isDefinedAt("base:class")) extract("PCLASS") else extract("MROLE")
    case "ISMINION" => Some(if(values.getOrElse("stat:hp","1") == "1") "Minion" else "Standard")
    case "COMMENT" => wrapExtract("text:comment")
    case _ => None
  }
  def extractGroup(group:String):Seq[StatBlockDataSource] = Nil
  
}
