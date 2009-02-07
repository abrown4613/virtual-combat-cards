//$Id$
package vcc

import scala.swing._
import scala.swing.event._ 
import vcc.util.swing._

import vcc.view.ViewCombatant
import scala.actors.Actor
import view.{SequenceTable,ViewCombatant}
import vcc.view.dialog.FileChooserHelper
import vcc.controller._

class MainMenu(coord:Coordinator,uia:Actor) extends MenuBar {
  
  lazy val encEditor=new vcc.view.dialog.EncounterEditorDialog(coord)
  
  var fileMenu=new Menu("File");
  fileMenu.contents += new MenuItem(Action("Load Party"){
    var file=FileChooserHelper.chooseOpenFile(this.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      coord.loader ! vcc.controller.actions.LoadPartyFile(file.get)
    }
  })
  val combatMenu = new Menu("Combat")
  combatMenu.contents +=new MenuItem(new Action("Go to First"){
    def apply():Unit={
      uia ! vcc.view.actor.GoToFirst()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('F'.toInt,java.awt.Event.CTRL_MASK))
  })
  combatMenu.contents += new Separator
  combatMenu.contents += new MenuItem(Action("Start Combat") {
    val diag=new vcc.view.dialog.InitiativeDialog(coord.tracker)
    diag.visible=true
  })
  combatMenu.contents += new MenuItem(Action("End Combat") {
    coord.tracker ! vcc.controller.actions.EndCombat()
    coord.tracker ! actions.Enumerate()
  })
  combatMenu.contents += new MenuItem(Action("Rest") {
    coord.tracker ! vcc.controller.actions.ApplyRest(false)
  })
  combatMenu.contents += new MenuItem(Action("Extended Rest") {
    coord.tracker ! vcc.controller.actions.ApplyRest(true)
  })
  
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(Action("Clear Monsters"){
    coord.tracker ! actions.ClearCombatants(false)
    coord.tracker ! actions.Enumerate()
  })
  combatMenu.contents +=new MenuItem(Action("Clear All"){
    coord.tracker ! actions.ClearCombatants(true)
    coord.tracker ! actions.Enumerate()
  })
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(new Action("Undo"){
    def apply():Unit={
      coord.tracker ! vcc.controller.actions.Undo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Z'.toInt,java.awt.Event.CTRL_MASK))
  })
  combatMenu.contents +=new MenuItem(new Action("Redo"){
    def apply():Unit={
      coord.tracker ! vcc.controller.actions.Redo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Y'.toInt,java.awt.Event.CTRL_MASK))
  })

  var viewMenu= new Menu("View")
  var hideDeadMenu=new CheckMenuItem("Hide Dead")
  hideDeadMenu.action=Action("Hide Dead"){
    uia ! vcc.view.actor.SetOption('HIDEDEAD,hideDeadMenu.peer.isSelected)
  }
  viewMenu.contents +=hideDeadMenu
  viewMenu.contents += new Separator
  viewMenu.contents += new MenuItem(Action("Encounter/Party Editor..."){
    encEditor.visible=true
  })
  
  contents+=fileMenu
  contents+=combatMenu
  contents+=viewMenu
}

object Main extends SimpleGUIApplication {
  var coord=vcc.controller.Coordinator.initialize

  var uia=new vcc.view.actor.UserInterface(coord.tracker)

  coord.start
  
  val commandPanel= new vcc.view.CombatantActionPanel(coord.tracker)
  var seqTable = new SequenceTable(uia)
  
  // Register panel with UIA
  uia.addSequenceListener(seqTable)
  uia.addSequenceListener(commandPanel)
  uia.addContextListner(commandPanel)
  uia.addContextListner(seqTable)
  uia.start
  coord.addObserver(uia)

  
  def top = new MainFrame {
    title = "Virtual Combat Cards"
    contents= new BorderPanel {
      add(commandPanel,BorderPanel.Position.East)
      add(seqTable,BorderPanel.Position.Center)
      add(new MainMenu(coord,uia),BorderPanel.Position.North)
    }
  }
  
}	