/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.compendium.view

import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import vcc.dnd4e.compendium._

object MonsterSummaryProjection extends TableModelRowProjection[MonsterSummary] {
  val columns: List[(String, java.lang.Class[_])] = List(
    ("Name", classOf[String]),
    ("Role", classOf[String]),
    ("Type", classOf[String]),
    ("Level", classOf[java.lang.Integer]),
    ("XP", classOf[java.lang.Integer])
  )
  val setter: PartialFunction[(Int, MonsterSummary, Any), Unit] = null

  def apply(col: Int, obj: MonsterSummary): java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.role
      case 2 => if (obj.minion) "Minion" else "Standard"
      case 3 => int2Integer(obj.level)
      case 4 => int2Integer(obj.xp)
    }
  }
}

object TrapSummaryProjection extends TableModelRowProjection[TrapSummary] {
  val columns: List[(String, java.lang.Class[_])] = List(
    ("Name", classOf[String]),
    ("Role", classOf[String]),
    ("Type", classOf[String]),
    ("Level", classOf[java.lang.Integer]),
    ("XP", classOf[java.lang.Integer])
  )
  val setter: PartialFunction[(Int, TrapSummary, Any), Unit] = null

  def apply(col: Int, obj: TrapSummary): java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.role
      case 2 => if (obj.hazard) "Hazard" else "Trap"
      case 3 => int2Integer(obj.level)
      case 4 => int2Integer(obj.xp)
    }
  }
}

object CharacterSummaryProjection extends TableModelRowProjection[CharacterSummary] {
  val columns: List[(String, java.lang.Class[_])] = List(
    ("Name", classOf[String]),
    ("Class", classOf[String]),
    ("Race", classOf[String]),
    ("Level", classOf[Int])
  )
  val setter: PartialFunction[(Int, CharacterSummary, Any), Unit] = null

  def apply(col: Int, obj: CharacterSummary): java.lang.Object = {
    col match {
      case 0 => obj.name
      case 1 => obj.cclass
      case 2 => obj.race
      case 3 => int2Integer(obj.level)
    }
  }
}


class CompendiumEntitySelectionPanel extends MigPanel("fill, ins 0,hidemode 1") with CompendiumRepositoryObserver {
  private val monsterButton = new RadioButton("Monster")
  private val characterButton = new RadioButton("Character")
  private val trapButton = new RadioButton("Trap")
  private val minLevelCombo = new ComboBox[Int]((1 to 40).toSeq)
  minLevelCombo.selection.item = 1
  private val maxLevelCombo = new ComboBox[Int]((1 to 40).toSeq)
  maxLevelCombo.selection.item = 40
  private val buttonGroup = new ButtonGroup(monsterButton, characterButton, trapButton)
  private val monsterTableModel = new ProjectionTableModel(MonsterSummaryProjection)
  private val monsterTable = new RowProjectionTable[EntitySummary]() {
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    model = monsterTableModel
    setColumnWidth(0, 150)
    setColumnWidth(3, 35)
    setColumnWidth(4, 60)
  }
  private val characterTableModel = new ProjectionTableModel(CharacterSummaryProjection)
  private val characterTable = new RowProjectionTable[EntitySummary]() {
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    model = characterTableModel
    setColumnWidth(0, 150)
    setColumnWidth(3, 35)
  }
  private val trapTableModel = new ProjectionTableModel(TrapSummaryProjection)
  private val trapTable = new RowProjectionTable[EntitySummary]() {
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    model = trapTableModel
    setColumnWidth(0, 150)
    setColumnWidth(3, 35)
  }
  private val scrollPane = new ScrollPane(monsterTable)
  monsterButton.selected = true

  private val activeEntityStore = Compendium.activeRepository
  activeEntityStore.registerObserver(this)

  refreshList()

  var doubleClickAction: Action = null

  val mouseAdapter = new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      if (e.getClickCount % 2 == 0 && doubleClickAction != null) doubleClickAction()
    }
  }
  monsterTable.peer.addMouseListener(mouseAdapter)
  characterTable.peer.addMouseListener(mouseAdapter)
  trapTable.peer.addMouseListener(mouseAdapter)

  add(monsterButton, "split 4")
  add(trapButton)
  add(characterButton, "wrap")
  add(new Label("Show level:"), "split 4")
  add(minLevelCombo, "gap unrel")
  add(new Label(" to"), "")
  add(maxLevelCombo, "wrap")
  add(scrollPane, "span 3,wrap, growx, growy")

  listenTo(monsterButton, characterButton, trapButton, minLevelCombo.selection, maxLevelCombo.selection)
  reactions += {
    case ButtonClicked(this.monsterButton) =>
      scrollPane.contents = monsterTable
    case ButtonClicked(this.characterButton) =>
      scrollPane.contents = characterTable
    case ButtonClicked(this.trapButton) =>
      scrollPane.contents = trapTable
    case SelectionChanged(this.minLevelCombo) =>
      val level = minLevelCombo.selection.item
      if (maxLevelCombo.selection.item < level) maxLevelCombo.selection.item = level
      refreshList()
    case SelectionChanged(this.maxLevelCombo) =>
      val level = maxLevelCombo.selection.item
      if (minLevelCombo.selection.item > level) minLevelCombo.selection.item = level
      refreshList()
  }

  def currentSelection: Option[EntitySummary] = {
    val activeTable = {
      if (monsterButton.selected) monsterTable
      else if (characterButton.selected) characterTable
      else trapTable
    }
    if (activeTable.selection.rows.isEmpty) None
    else Some(activeTable.model.asInstanceOf[ProjectionTableModel[EntitySummary]].content(activeTable.selection.rows.toSeq(0)))
  }

  def sortedSummary[T](el: Seq[T], filter: T => Boolean, comp: (T, T) => Boolean)(implicit manifest: Manifest[T]): Seq[T] = {
    import scala.util.Sorting.stableSort
    val filtered = el.filter(filter)
    stableSort(filtered, comp).toSeq
  }


  def refreshList() {
    val levelRange = (minLevelCombo.selection.item to maxLevelCombo.selection.item)
    monsterTableModel.content = sortedSummary[MonsterSummary](Compendium.activeRepository.getMonsterSummaries(),
      x => levelRange.contains(x.level), (x, y) => x.name < y.name)
    characterTableModel.content = sortedSummary[CharacterSummary](Compendium.activeRepository.getCharacterSummaries(),
      x => levelRange.contains(x.level), (x, y) => x.name < y.name)
    trapTableModel.content = sortedSummary[TrapSummary](Compendium.activeRepository.getTrapSummaries(),
      x => levelRange.contains(x.level), (x, y) => x.name < y.name)
  }

  def compendiumChanged() {
    refreshList()
  }
}
