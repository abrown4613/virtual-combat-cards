/**
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
//$Id$
package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.infra.datastore.naming.{EntityID, DataStoreURI}

import vcc.dnd4e.domain.compendium._
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.dnd4e.model.{PartyMember, PartyFile}
import vcc.model.Registry
import vcc.dnd4e.view.helper.PartyLoader
import vcc.dnd4e.tracker.common.CombatantID
import vcc.dnd4e.view.{IconLibrary, PanelDirector}
import annotation.tailrec
import java.io.FileInputStream

/**
 * Helper methods for XP operations.
 */
object ExperienceCalculator {
  private val incrementList = List(25, 50, 100, 200, 400, 950, 2000)

  /**
   * Returns the level of an encounter from total XP and the party size.
   * @param partySize Number of character in the party (if less then 1, will assume 1)
   * @param xp Total XP of the encounter 
   * @return Encounter level
   */
  def encounterLevel(partySize: Int, xp: Int): Int = {
    @tailrec def levelFromXP_r(x: Int, l: Int, il: List[Int]): Int = {
      if (x > 4 * il.head) levelFromXP_r(x - 4 * il.head, l + 4, if (il.tail == Nil) List(il.head * 2) else il.tail)
      else l + ((x - 1) / il.head)
    }

    val xppp = xp / (if (partySize < 1) 1 else partySize)
    if (xppp <= 100) 1
    else levelFromXP_r(xppp - 100, 2, incrementList)
  }
}

class PartyEditor(director: PanelDirector) extends Frame {

  class PartyTableEntry(val eid: EntityID, val name: String, var alias: String, var id: String, var qty: Int, val xp: Int) extends Ordered[PartyTableEntry] {
    def toIndividual(): Iterable[PartyTableEntry] = (if (qty == 1) Seq(this)
    else (1 to qty).map(e => new PartyTableEntry(eid, name, alias, id, 1, xp)))

    def toPartyMember(): PartyMember = PartyMember(if (id != null) CombatantID(id) else null, alias, eid)

    def compare(that: PartyTableEntry) = this.name.compare(that.name)
  }

  object PartyTableEntryProject extends TableModelRowProjection[PartyTableEntry] {
    val columns: List[(String, java.lang.Class[_])] = List(
      ("ID", classOf[String]),
      ("Name", classOf[String]),
      ("Alias/Mini", classOf[String]),
      ("Qty", classOf[java.lang.Integer]),
      ("XP", classOf[java.lang.Integer])
    )
    val setter: PartialFunction[(Int, PartyTableEntry, Any), Unit] = {
      case (0, entry, value) if (entry.qty == 1) =>
        entry.id = value.toString
        if (entry.id == "") entry.id = null
      case (2, entry, value) => entry.alias = value.toString
      case (3, entry, value) if (entry.id == null) =>
        val v = try {
          value.toString.toInt
        } catch {
          case _ => 1
        }
        if (v > 0) {
          entry.qty = v
          fireQuantityChange()
        }
    }

    def apply(col: Int, obj: PartyTableEntry): java.lang.Object = {
      col match {
        case 0 => obj.id
        case 1 => obj.name
        case 2 => obj.alias
        case 3 => int2Integer(obj.qty)
        case 4 => int2Integer(obj.xp)
      }
    }
  }

  title = "Edit/Encounter Party"
  iconImage = IconLibrary.MetalD20.getImage

  private val partyTableModel = new ProjectionTableModel[PartyTableEntry](PartyTableEntryProject)
  private val table = new RowProjectionTable[PartyTableEntry]() {
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    model = partyTableModel
    setColumnWidth(0, 30, 45, 60)
    setColumnWidth(1, 150)
    setColumnWidth(2, 100)
    setColumnWidth(3, 30, 45, 60)
    setColumnWidth(4, 50, 75, 100)
  }

  private val compendiumEntries = new CompendiumEntitySelectionPanel
  private val totalXPLabel = new Label()
  private val partySizeCombo = new ComboBox((1 to 10).toSeq)
  partySizeCombo.selection.item = 5
  recalculateXP(Nil)

  private def entitySummaryToPartyEntry(es: EntitySummary): PartyTableEntry =
    es match {
      case m: MonsterSummary => new PartyTableEntry(m.eid, m.name, null, null, 1, m.xp)
      case m: TrapSummary => new PartyTableEntry(m.eid, m.name, null, null, 1, m.xp)
      case c: CharacterSummary => new PartyTableEntry(c.eid, c.name, null, null, 1, 0)
      case s: EntitySummary => throw new Exception("Unexpected EntitySummary class: " + s.classid)
      case null => null
    }

  private val addButton = new Button(Action("Add to Party >>") {
    val sel = compendiumEntries.currentSelection
    if (sel.isDefined) {
      val nl = compressEntries(partyTableModel.content ++ Seq(entitySummaryToPartyEntry(sel.get)))
      recalculateXP(nl)
      partyTableModel.content = nl
    }
  })

  compendiumEntries.doubleClickAction = addButton.action

  private val removeButton = new Button(Action(" << Remove") {
    val sel = table.selection.rows.toSeq
    if (!sel.isEmpty) {
      SwingHelper.invokeLater {
        val idx = sel.toSeq(0)
        val l = partyTableModel.content.toList
        val nl = l filterNot (_ == l(idx))
        partyTableModel.content = nl
        if (!nl.isEmpty) table.selection.rows += (if (idx < nl.length) idx else nl.length - 1)
        recalculateXP(nl)
      }
    }
  })

  private val clearAllButton = new Button(Action("Clear all") {
    partyTableModel.content = Nil
  })

  private val collapseCheckBox = new CheckBox("Collapse similar entries")

  contents = new MigPanel("fill,flowy", "[350][fill,growprio 0][350,fill]", "[]") {
    add(compendiumEntries, "growy, growx,wrap")
    add(addButton, "split 3")
    add(removeButton, "growx")
    add(clearAllButton, "growx,wrap")
    add(new MigPanel("fill,ins 0") {
      add(new Label("Party Size:"), "split 3")
      add(partySizeCombo, "")
      add(totalXPLabel, "gap 20px, align right, wrap")
      add(collapseCheckBox, "wrap")
      add(new ScrollPane(table), "span 2,grow")
    }, "grow")
  }

  menuBar = {
    val mb = new MenuBar()
    val fmenu = new Menu("File")
    mb.contents += fmenu
    fmenu.contents += new MenuItem(Action("Save ...") {
      doSave()
    })
    fmenu.contents += new MenuItem(Action("Load ...") {
      doLoad()
    })
    fmenu.contents += new Separator()
    fmenu.contents += new MenuItem(Action("Add to combat") {
      doAddToCombat()
    })
    mb
  }

  // Listen logic

  listenTo(collapseCheckBox, partySizeCombo.selection)

  reactions += {
    case ButtonClicked(this.collapseCheckBox) =>
      partyTableModel.content = compressEntries(partyTableModel.content)
    case SelectionChanged(this.partySizeCombo) =>
      fireQuantityChange()

  }

  private def doSave() {
    val file = FileChooserHelper.chooseSaveFile(table.peer, FileChooserHelper.partyFilter)
    if (file.isDefined) {
      val pml = expandEntries(partyTableModel.content).map(_.toPartyMember())
      PartyFile.saveToFile(file.get, pml)
    }
  }

  private def doLoad() {
    val file = FileChooserHelper.chooseOpenFile(table.peer, FileChooserHelper.partyFilter)
    if (file.isDefined) {
      val es = Registry.get[CompendiumRepository](Registry.get[DataStoreURI]("Compendium").get).get
      val combs = PartyLoader.getInstance(null, menuBar).validatePartyLoadAndWarn(PartyFile.loadFromStream(new FileInputStream(file.get)))
      val pml = compressEntries(combs.map(pm => {
        //Load summary, convert and copy extra data
        val pe = entitySummaryToPartyEntry(es.getEntitySummary(pm.eid))
        if (pe != null) {
          pe.id = if (pm.id != null) pm.id.id else null
          pe.alias = pm.alias
          pe
        } else null
      }).filter(_ != null))
      SwingHelper.invokeLater {
        partyTableModel.content = pml
        recalculateXP(pml)
      }
    }
  }

  private def doAddToCombat() {
    PartyLoader.getInstance(director, this.menuBar).loadToBattle(expandEntries(partyTableModel.content).map(_.toPartyMember))
  }

  private def expandEntries(ol: Seq[PartyTableEntry]): Seq[PartyTableEntry] = ol.flatMap(x => x.toIndividual())

  private def compressEntries(ol: Seq[PartyTableEntry]): Seq[PartyTableEntry] = {
    val ul = if (!collapseCheckBox.selected) {
      expandEntries(ol)
    } else {
      var map = scala.collection.mutable.Map.empty[(EntityID, String, String), PartyTableEntry]
      for (entry <- ol) {
        val key = (entry.eid, entry.id, entry.alias)
        if (map.isDefinedAt(key)) map(key).qty += entry.qty
        else map += (key -> entry)
      }
      map.map(x => x._2).toSeq
    }
    scala.util.Sorting.stableSort(ul, (y: PartyTableEntry) => y).toSeq
  }

  private def fireQuantityChange() {
    SwingHelper.invokeLater {
      val pml = compressEntries(partyTableModel.content)
      partyTableModel.content = pml
      recalculateXP(pml)
    }
  }

  private def recalculateXP(pml: Seq[PartyTableEntry]) {
    val xp = pml.map(e => e.qty * e.xp).foldLeft(0)(_ + _)
    val level = ExperienceCalculator.encounterLevel(partySizeCombo.selection.item, xp)
    totalXPLabel.text = "Total XP: " + xp + " Level: " + level
  }
}
