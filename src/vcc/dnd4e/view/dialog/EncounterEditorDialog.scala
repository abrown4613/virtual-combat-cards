//$Id$
package vcc.dnd4e.view.dialog

import scala.swing._
import scala.swing.event._

import vcc.dnd4e.model.{CombatantType,PartyLoader,CombatantTemplate}

class EncounterEditorDialog(val coord:vcc.controller.Coordinator) extends Frame {
  title = "Party/Encounter Editor"
  
  private val entries=new vcc.util.swing.ProjectionTableModel[EncounterEditorTableEntry](EncounterEditorTableEntryProjection)
  //Buttons
  private val btnAddMonster=new Button("Add Monster")
  private val btnAddMinion=new Button("Add Minion")
  private val btnAddCharacter=new Button("Add Character")
  private val btnRemoveCombatant=new Button("Remove Combatant")
  private val btnExpandSame=new Button("Expand Similar")
  private val btnCompressSame=new Button("Compress Similar")
  private val btnClearAll=new Button("Clear All")
  private val btns=List(btnAddMonster,btnAddMinion,btnAddCharacter,btnRemoveCombatant,btnExpandSame,btnCompressSame,btnClearAll)
  
  iconImage=IconLibrary.MetalD20.getImage
  
  private val table=new vcc.util.swing.EnhancedTable() {
    model=entries
    autoResizeMode=Table.AutoResizeMode.Off
    for(i<-0 to 8)
      setColumnWidth(i,if(i!=2)35 else 150)
    override def rendererComponentFix(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
      var c=super.rendererComponentFix(isSelected,hasFocus,row,column)
      if(c.isInstanceOf[javax.swing.JLabel]) {
        c.asInstanceOf[javax.swing.JLabel].setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
      }
      c
    }
  }
  
  override val menuBar=new MenuBar
  val menuFile=new Menu("File")
  menuFile.contents+= new MenuItem(Action("Save..."){ doSave()})
  menuFile.contents+= new MenuItem(Action("Load..."){ doLoad()})
  menuFile.contents+= new MenuItem(Action("Add to battle"){ doAddToBattle()}) {
    enabled=(coord!=null)
  }
  menuFile.contents+= new Separator
  menuFile.contents+= new MenuItem(Action("Close"){ visible=false})
  menuBar.contents+=menuFile
  
  contents = new BorderPanel {
    add(new BoxPanel(Orientation.Vertical) {
      border=javax.swing.BorderFactory.createTitledBorder("Operations")
      contents+=new GridPanel(7,1) {
        this.vGap=2
        this.hGap=2
        contents ++ btns
        maximumSize=(new java.awt.Dimension(200,210))
      }
      contents+=new Label("")
    },BorderPanel.Position.East)
    add(new ScrollPane{
      border=javax.swing.BorderFactory.createLoweredBevelBorder
      this.peer.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
      contents=table
    }, BorderPanel.Position.Center)
    add(menuBar,BorderPanel.Position.North)
  }
  
  listenTo(btns: _*)
  
  reactions += {
    case ButtonClicked(this.btnRemoveCombatant) =>
      var es=entries.content
      var sel=table.selection.rows.toList
      table.selection.rows.clear
      var lst=for(i<- 0 to es.size-1; if(!sel.contains(i)))
        yield(es(i)) 
      entries.content=lst.toSeq
      table.repaint
     
    case ButtonClicked(this.btnAddMinion) => addEntry(CombatantType.Minion)
    case ButtonClicked(this.btnAddMonster) => addEntry(CombatantType.Monster)
    case ButtonClicked(this.btnAddCharacter) => addEntry(CombatantType.Character)
    case ButtonClicked(this.btnCompressSame) => entries.content=compressEntries(entries.content.toList)
    case ButtonClicked(this.btnExpandSame) => entries.content=expandEntries(entries.content.toList)
    case ButtonClicked(this.btnClearAll) => entries.content=Nil
  }
  
  private def addEntry(ctype: CombatantType.Value) {
    var v=(new EncounterEditorTableEntry(ctype)) 
    entries.content = (Seq(v) ++ entries.content)
  }
  
  private def doSave() {
    var file=FileChooserHelper.chooseSaveFile(table.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      var cel= expandEntries(entries.content.toList).map(_.toSingleCombatantTemplate.toXML)
      var doc=new scala.xml.Elem(null,"party",scala.xml.Null,scala.xml.TopScope,cel: _*)
      scala.xml.XML.saveFull(file.get.getAbsolutePath,doc,"UTF-8",true,null)
    }
  }
  
  private def doLoad() {
    var file=FileChooserHelper.chooseOpenFile(table.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      var combs=PartyLoader.loadFromFile(file.get)
      entries.content=compressEntries(combs map EncounterEditorTableEntry.fromCombatantTemplate)
    }
  }
  
  private def doAddToBattle() {
    if(coord!=null) {
      var cel= expandEntries(entries.content.toList).map(_.toSingleCombatantTemplate)
      coord.loader ! vcc.controller.actions.LoadPartyFromTemplate(cel)
    }
  }
  private def compressEntries(cl:List[EncounterEditorTableEntry]):List[EncounterEditorTableEntry] = {
    if(!cl.isEmpty) {
      var nl=compressEntries_r(Nil,cl.head)
      for(e<-cl.tail) { 
        nl=compressEntries_r(nl,e)
      }
      nl
    } else 
      cl
  }
  
  private def compressEntries_r(cl:List[EncounterEditorTableEntry],e:EncounterEditorTableEntry): List[EncounterEditorTableEntry] = {
    cl match {
      case Nil => List(e)
      case he :: r if(he isSame e)=> he.qty += e.qty; he :: r
      case h :: r => h :: compressEntries_r(r,e)
    } 
  }
  
  def compressEntries(cl:List[(Int,CombatantTemplate)],ne:CombatantTemplate):List[(Int,CombatantTemplate)] = {
   cl match {
     case Nil => List((1,ne))
     case (hc,he) :: r if(he isSame ne)=> (hc+1,he) :: r
     case h :: r => h :: compressEntries(r,ne)
   } 
  }

  private def expandEntries(cl:List[EncounterEditorTableEntry]):List[EncounterEditorTableEntry] = {
    cl match {
      case Nil => Nil
      case h :: r if(h.qty>1)=> ((1 to h.qty) map (x=>h.singleCopy)).toList ::: expandEntries(r)
      case h :: r => h :: expandEntries(r)
    }
  }
  
}
