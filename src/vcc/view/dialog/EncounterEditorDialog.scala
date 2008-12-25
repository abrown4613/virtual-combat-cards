//$Id$
package vcc.view.dialog

import scala.swing._
import scala.swing.event._

import vcc.model.CombatantType

object EncounterEditorDialog extends Frame {
  title = "Party/Encounter Editor"
  
  val entries=new vcc.util.swing.ProjectionTableModel[EncounterEditorTableEntry](EnconterEditorTableEntryProjection)
  //Buttons
  val btnAddMonster=new Button("Add Monster")
  val btnAddMinion=new Button("Add Minion")
  val btnAddCharacter=new Button("Add Character")
  val btnRemoveCombatant=new Button("Remove Combatant")
  val btns=List(btnAddMonster,btnAddMinion,btnAddCharacter,btnRemoveCombatant)
  
  val table=new vcc.util.swing.EnhancedTable() {
    model=entries
    autoResizeMode=Table.AutoResizeMode.Off
    for(i<-0 to 7)
      setColumnWidth(i,if(i!=2)35 else 150)
    override def rendererComponentFix(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
      var c=super.rendererComponentFix(isSelected,hasFocus,row,column)
      if(c.isInstanceOf[javax.swing.JLabel]) {
        c.asInstanceOf[javax.swing.JLabel].setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
      }
      c
    }
  }
  
  entries.content= List(
    new EncounterEditorTableEntry(CombatantType.Character),
    new EncounterEditorTableEntry(CombatantType.Minion),
    new EncounterEditorTableEntry(CombatantType.Monster)
  )
  contents = new BorderPanel {
    add(new BoxPanel(Orientation.Vertical) {
      border=javax.swing.BorderFactory.createTitledBorder("Operations")
      contents+=new GridPanel(5,1) {
        this.vGap=2
        this.hGap=2
        contents ++ btns
        maximumSize=(new java.awt.Dimension(200,120))
      }
      contents+=new Label("")
    },BorderPanel.Position.East)
    add(new ScrollPane{
      border=javax.swing.BorderFactory.createLoweredBevelBorder
      this.peer.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
      contents=table
    }, BorderPanel.Position.Center)
    add(new MenuBar {
          contents+=(new Menu("File"))
        },BorderPanel.Position.North)
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
  }
  
  private def addEntry(ctype: CombatantType.Value) {
    var v=(new EncounterEditorTableEntry(ctype)) 
    entries.content = (Seq(v) ++ entries.content)
  } 
}
