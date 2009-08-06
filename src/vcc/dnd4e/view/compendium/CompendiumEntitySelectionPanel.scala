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

import scala.swing._
import scala.swing.event._
import vcc.util.swing._ 

import vcc.model.datastore.{EntityID,EntitySummary,EntityStore,EntityStoreID}
import vcc.model.Registry
import vcc.dnd4e.model.{MonsterSummary,CharacterSummary,Compendium}

object MonsterSummaryProjection extends TableModelRowProjection[MonsterSummary] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Name",classOf[String]),
    ("Role",classOf[String]),
    ("Type",classOf[String]),
    ("Level",classOf[Integer]),
    ("XP",classOf[Integer]),
    ("EID",classOf[String])
  )
  val setter:PartialFunction[(Int,MonsterSummary,Any),Unit] = null
  def apply(col:Int,obj:MonsterSummary):java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.role
      case 2 => if(obj.minion) "Minion" else "Standard"
      case 3 => int2Integer(obj.level)
      case 4 => int2Integer(obj.xp)
      case 5 => obj.eid
    }
  }
}

object CharacterSummaryProjection extends TableModelRowProjection[CharacterSummary] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Name",classOf[String]),
    ("Level",classOf[Int]),
    ("Class",classOf[String]),
    ("Race",classOf[String]),
    ("EID",classOf[String])
  )
  val setter:PartialFunction[(Int,CharacterSummary,Any),Unit] = null
  def apply(col:Int,obj:CharacterSummary):java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => int2Integer(obj.level)
      case 2 => obj.cclass
      case 3 => obj.race
      case 4 => obj.eid 
    }
  }
}

class CompendiumEntitySelectionPanel extends MigPanel("fill, ins 0"){
  private val monsterButton = new RadioButton("Monster")
  private val characterButton = new RadioButton("Character")
  private val buttonGroup = new ButtonGroup(monsterButton,characterButton)
  private val table = new RowProjectionTable[EntitySummary]() {
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
  }
  private val monsterTableModel = new ProjectionTableModel(MonsterSummaryProjection) 
  private val characterTableModel = new ProjectionTableModel(CharacterSummaryProjection) 
 
  private val activeEntityStore = Registry.get[EntityStore](Registry.get[EntityStoreID]("Compendium").get).get
 
  println(activeEntityStore)
  monsterTableModel.content = activeEntityStore.enumerate(Compendium.monsterClassID).map(eid => activeEntityStore.loadEntitySummary(eid).asInstanceOf[MonsterSummary]).toSeq
  characterTableModel.content = activeEntityStore.enumerate(Compendium.characterClassID).map(eid => activeEntityStore.loadEntitySummary(eid).asInstanceOf[CharacterSummary]).toSeq 
  table.model = monsterTableModel
  
  add(monsterButton,"split 4")
  add(characterButton,"wrap")
  add(new ScrollPane(table), "span 3, growx, growy")
  
  listenTo(monsterButton,characterButton) 
  reactions += {
    case ButtonClicked(this.monsterButton) =>
      table.model = monsterTableModel
      table.repaint()
    case ButtonClicked(this.characterButton) =>
      table.model = characterTableModel
      table.repaint()
  }
  
  def currentSelection():Option[EntitySummary] = 
    if(table.selection.rows.isEmpty) None 
    else Some(table.model.asInstanceOf[ProjectionTableModel[EntitySummary]].content(table.selection.rows.toSeq(0)))

}
