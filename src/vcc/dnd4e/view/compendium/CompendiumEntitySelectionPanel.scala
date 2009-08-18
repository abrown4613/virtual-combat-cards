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

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import vcc.model.datastore.{EntityID,EntitySummary,EntityStore,EntityStoreID,EntityClassID}
import vcc.model.Registry
import vcc.dnd4e.model.{MonsterSummary,CharacterSummary,Compendium}

object MonsterSummaryProjection extends TableModelRowProjection[MonsterSummary] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Name",classOf[String]),
    ("Role",classOf[String]),
    ("Type",classOf[String]),
    ("Level",classOf[Integer]),
    ("XP",classOf[Integer])
  )
  val setter:PartialFunction[(Int,MonsterSummary,Any),Unit] = null
  def apply(col:Int,obj:MonsterSummary):java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.role
      case 2 => if(obj.minion) "Minion" else "Standard"
      case 3 => int2Integer(obj.level)
      case 4 => int2Integer(obj.xp)
    }
  }
}

object CharacterSummaryProjection extends TableModelRowProjection[CharacterSummary] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Name",classOf[String]),
    ("Class",classOf[String]),
    ("Race",classOf[String]),
    ("Level",classOf[Int])
  )
  val setter:PartialFunction[(Int,CharacterSummary,Any),Unit] = null
  def apply(col:Int,obj:CharacterSummary):java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.cclass
      case 2 => obj.race
      case 3 => int2Integer(obj.level)
    }
  }
}


class CompendiumEntitySelectionPanel extends MigPanel("fill, ins 0,hidemode 1"){
  private val monsterButton = new RadioButton("Monster")
  private val characterButton = new RadioButton("Character")
  private val buttonGroup = new ButtonGroup(monsterButton,characterButton)
  private val monsterTableModel = new ProjectionTableModel(MonsterSummaryProjection) 
  private val monsterTable = new RowProjectionTable[EntitySummary]() {
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    model = monsterTableModel
    setColumnWidth(0,150)
    setColumnWidth(3,35)
    setColumnWidth(4,60)
  }
  private val characterTableModel = new ProjectionTableModel(CharacterSummaryProjection) 
  private val characterTable = new RowProjectionTable[EntitySummary]() {
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    model = characterTableModel
    setColumnWidth(0,150)
    setColumnWidth(3,35)
  }
  private val scrollPane = new ScrollPane(monsterTable)
  monsterButton.selected = true
 
  private val activeEntityStore = Registry.get[EntityStore](Registry.get[EntityStoreID]("Compendium").get).get
 
  refreshList()

  var doubleClickAction:Action = null

  val mouseAdapter = new MouseAdapter(){
	override def mouseClicked(e: java.awt.event.MouseEvent){
	  if (e.getClickCount() == 2 && doubleClickAction != null) doubleClickAction()
    }}
  monsterTable.peer.addMouseListener(mouseAdapter)
  characterTable.peer.addMouseListener(mouseAdapter)

  add(monsterButton,"split 4")
  add(characterButton,"wrap")
  add(scrollPane, "span 3,wrap, growx, growy")
  //add(characterScrollPane, "span 3,wrap, growx, growy")
  
  listenTo(monsterButton,characterButton,monsterTable) 
  reactions += {
    case ButtonClicked(this.monsterButton) =>
      scrollPane.contents = monsterTable
    case ButtonClicked(this.characterButton) =>
      scrollPane.contents = characterTable
  }
  
  def currentSelection():Option[EntitySummary] = {
    val activeTable = if(monsterButton.selected) monsterTable else characterTable
    if(activeTable.selection.rows.isEmpty) None 
    else Some(activeTable.model.asInstanceOf[ProjectionTableModel[EntitySummary]].content(activeTable.selection.rows.toSeq(0)))
  }

  def entitiesFilteredAndSorted[T](cid: EntityClassID, comp:(T,T)=>Boolean):Seq[T] = {
    val el = activeEntityStore.enumerate(cid).toList.map(eid => activeEntityStore.loadEntitySummary(eid).asInstanceOf[T]).filter(x=> x!=null)
    scala.util.Sorting.stableSort(el,comp).toSeq
  }
  
  def refreshList() {
	monsterTableModel.content = entitiesFilteredAndSorted[MonsterSummary](Compendium.monsterClassID,(x,y)=> x.name < y.name)
	characterTableModel.content = entitiesFilteredAndSorted[CharacterSummary](Compendium.characterClassID,(x,y)=> x.name < y.name) 
  }
  
}
