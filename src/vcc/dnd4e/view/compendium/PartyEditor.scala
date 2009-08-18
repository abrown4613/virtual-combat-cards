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
import vcc.model.datastore.{EntityID,EntitySummary,EntityStoreID,EntityStore}

import vcc.dnd4e.model.{MonsterSummary,CharacterSummary}
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.dnd4e.model.{PartyMember,PartyLoader}
import vcc.model.Registry

class PartyEditor extends Frame {
  
  class PartyTableEntry(val eid:EntityID, val name: String, var alias:String, var id:String, var qty:Int, val xp:Int) extends Ordered[PartyTableEntry] {
    def toIndividual():Iterable[PartyTableEntry] = (if(qty == 1) Seq(this)
                                                 else (1 to qty).map(e => new PartyTableEntry(eid,name,alias,id,1,xp)))
    
    def toPartyMember():PartyMember = PartyMember(if(id!=null) Symbol(id)else null, alias, eid)
    
    def compare(that:PartyTableEntry) = this.name.compare(that.name)
  }
  
  object PartyTableEntryProject extends TableModelRowProjection[PartyTableEntry] {
    val columns:List[(String,java.lang.Class[_])] = List(
      ("ID",classOf[String]),
      ("Name",classOf[String]),
      ("Alias/Mini",classOf[String]),
      ("Qty",classOf[Integer]),
      ("XP",classOf[Integer])
    )
    val setter:PartialFunction[(Int,PartyTableEntry,Any),Unit] = {
      case (0,entry,value) if(entry.qty == 1) => 
        entry.id = value.toString
        if(entry.id == "") entry.id = null
      case (2,entry,value) => entry.alias = value.toString
      case (3,entry,value) if(entry.id == null) => 
        val v = try { value.toString.toInt } catch { case _ => 1 }
        if(v>0) { 
          entry.qty = v
          fireQuantityChange()
        }
    }
    def apply(col:Int,obj:PartyTableEntry):java.lang.Object = {
      col match {
      	case 0 => obj.id
      	case 1 => obj.name
      	case 2 => obj.alias
      	case 3 => int2Integer(obj.qty)
      	case 4 => int2Integer(obj.xp)
      }
    }
  }
  
  title = "Edit Party"
  iconImage = IconLibrary.MetalD20.getImage

  
  private val partyTableModel = new ProjectionTableModel[PartyTableEntry](PartyTableEntryProject)
  private val table = new RowProjectionTable[PartyTableEntry]() {
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    model = partyTableModel
    setColumnWidth(0,30,45,60)
    setColumnWidth(1,150)
    setColumnWidth(2,100)
    setColumnWidth(3,30,45,60)
    setColumnWidth(4,50,75,100)
  }

  private val compendiumEntries = new CompendiumEntitySelectionPanel
  private val totalXPLabel = new Label()
  recalculateXP(Nil)
  
  private def entitySummaryToPartyEntry(es:EntitySummary):PartyTableEntry = 
    es match {
        case m: MonsterSummary => new PartyTableEntry(m.eid,m.name,null,null,1,m.xp)
        case c: CharacterSummary => new PartyTableEntry(c.eid,c.name,null,null,1,0)
        case s: EntitySummary => throw new Exception("Unexpected EntitySummary"+s.classid)
    }
  
  private val addButton = new Button(Action("Add to Party >>"){ 
    val sel = compendiumEntries.currentSelection
    if(sel.isDefined) {
      val nl = compressEntries(partyTableModel.content ++ Seq(entitySummaryToPartyEntry(sel.get)))
      recalculateXP(nl)
      partyTableModel.content = nl 
    }
  })
  
  compendiumEntries.doubleClickAction = addButton.action
  
  private val removeButton = new Button(Action(" << Remove"){
	val sel = table.selection.rows.toSeq
	if(!sel.isEmpty) {
	  val idx = sel.toSeq(0)
	  val l = partyTableModel.content.toList
	  val nl = l - l(idx)
	  partyTableModel.content = nl
      recalculateXP(nl)
	}
  })
  
  private val collapseCheckBox = new CheckBox("Collapse similar entries")
  
  contents = new MigPanel("fill,flowy","[350][fill,growprio 0][350,fill]","[]") {
    add(compendiumEntries ,"growy, growx,wrap")
	add(addButton,"split 2")
	add(removeButton,"growx,wrap")
	add(new MigPanel("fill,ins 0") {
	  add(collapseCheckBox,"")
	  add(totalXPLabel, "wrap, align right")
	  add(new ScrollPane(table),"span 2,grow")
	},"grow")
  }
  
  menuBar = { 
    val mb = new MenuBar()
    val fmenu = new Menu("File")
    mb.contents += fmenu
    fmenu.contents += new MenuItem(Action("Save ..."){ doSave()})
    fmenu.contents += new MenuItem(Action("Load ..."){ doLoad()})
    fmenu.contents += new Separator()
    fmenu.contents += new MenuItem(Action("Add to combat"){ doAddToCombat()})
    mb
  }
  
  // Listen logic
  
  listenTo(collapseCheckBox)
  
  reactions += {
    case ButtonClicked(this.collapseCheckBox) => 
      partyTableModel.content =  compressEntries(partyTableModel.content) 
  }
  
  
  private def doSave() {
    var file=FileChooserHelper.chooseSaveFile(table.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      val pml = expandEntries(partyTableModel.content).map(_.toPartyMember())
      PartyLoader.saveToFile(null,file.get,pml)
    }    
  }
  
  private def doLoad() {
    var file=FileChooserHelper.chooseOpenFile(table.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      val es = Registry.get[EntityStore](Registry.get[EntityStoreID]("Compendium").get).get
      var combs=PartyLoader.loadFromFile(Registry.get[EntityStoreID]("Compendium").get,file.get)
      val pml = compressEntries(combs.map(pm => {
        //Load summary, convert and copy extra data
        val pe = entitySummaryToPartyEntry(es.loadEntitySummary(pm.eid))
        pe.id = if(pm.id!=null )pm.id.name else null
        pe.alias = pm.alias
        pe
      }))
      println(pml.map(x=>x.name))
      partyTableModel.content = pml
      recalculateXP(pml)
    }
  }
  
  private def doAddToCombat() {
    val esid = Registry.get[EntityStoreID]("Compendium").get
	PartyLoader.loadToBattle(esid,expandEntries(partyTableModel.content).map(_.toPartyMember))
  }

  private def expandEntries(ol:Seq[PartyTableEntry]):Seq[PartyTableEntry] = ol.flatMap[PartyTableEntry](x => x.toIndividual()) 
  
  private def compressEntries(ol:Seq[PartyTableEntry]):Seq[PartyTableEntry] = {
    val ul = if(!collapseCheckBox.selected) {
      expandEntries(ol)
    } else {
      var map = scala.collection.mutable.Map.empty[(EntityID,String,String),PartyTableEntry]
      for(entry <- ol) {
       val key = (entry.eid,entry.id,entry.alias)
       if(map.isDefinedAt(key)) map(key).qty += entry.qty
       else map += (key -> entry)
      }
      map.map(x => x._2).toSeq
    }
    scala.util.Sorting.stableSort(ul,(y:PartyTableEntry)=> y).toSeq
  }
  
  private def fireQuantityChange() {
    val pml = compressEntries(partyTableModel.content)
    partyTableModel.content = pml
    recalculateXP(pml)
  }
  
  private def recalculateXP(pml:Seq[PartyTableEntry]) {
    SwingHelper.invokeLater {
	  val xp = pml.map(e => e.qty * e.xp).foldLeft(0)(_ + _ )
	  totalXPLabel.text = "Total XP: "+xp
	}
  }
  
}
