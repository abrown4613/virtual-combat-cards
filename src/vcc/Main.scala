//$Id$
package vcc

import scala.swing._
import scala.swing.event._ 
import vcc.util.swing._

import scala.actors.Actor

import vcc.dnd4e.view.ViewCombatant
import dnd4e.view.{SequenceTable,ViewCombatant,StatusBar}
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.controller._
import vcc.dnd4e.controller.request._

class MainMenu(coord:Coordinator,uia:Actor) extends MenuBar {
  
  lazy val encEditor=new vcc.dnd4e.view.dialog.EncounterEditorDialog(coord)
  
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
      uia ! vcc.dnd4e.view.actor.GoToFirst()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('F'.toInt,java.awt.Event.CTRL_MASK))
  })
  combatMenu.contents += new Separator
  combatMenu.contents += new MenuItem(Action("Start Combat") {
    val diag=new vcc.dnd4e.view.dialog.InitiativeDialog(coord.tracker)
    diag.visible=true
  })
  combatMenu.contents += new MenuItem(Action("End Combat") {
    coord.tracker ! EndCombat()
    coord.tracker ! Enumerate()
  })
  combatMenu.contents += new MenuItem(Action("Rest") {
    coord.tracker ! ApplyRest(false)
  })
  combatMenu.contents += new MenuItem(Action("Extended Rest") {
    coord.tracker ! ApplyRest(true)
  })
  
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(Action("Clear Monsters"){
    coord.tracker ! ClearCombatants(false)
    coord.tracker ! Enumerate()
  })
  combatMenu.contents +=new MenuItem(Action("Clear All"){
    coord.tracker ! ClearCombatants(true)
    coord.tracker ! Enumerate()
  })
  
  val historyMenu= new Menu("History")
  historyMenu.contents +=new MenuItem(new Action("Undo"){
    def apply():Unit={
      coord.tracker ! vcc.controller.actions.Undo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Z'.toInt,java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents +=new MenuItem(new Action("Redo"){
    def apply():Unit={
      coord.tracker ! vcc.controller.actions.Redo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Y'.toInt,java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new Separator
  historyMenu.contents +=new MenuItem(Action("Clear History"){
    coord.tracker ! actions.ClearTransactionLog()
  })
  
  var viewMenu= new Menu("View")
  var hideDeadMenu=new CheckMenuItem("Hide Dead")
  hideDeadMenu.action=Action("Hide Dead"){
    uia ! vcc.dnd4e.view.actor.SetOption('HIDEDEAD,hideDeadMenu.peer.isSelected)
  }
  viewMenu.contents +=hideDeadMenu
  viewMenu.contents += new Separator
  viewMenu.contents += new MenuItem(Action("Encounter/Party Editor..."){
    encEditor.visible=true
  })
  
  contents+=fileMenu
  contents+=combatMenu
  contents+=historyMenu
  contents+=viewMenu
}

object Main extends SimpleGUIApplication {
  import vcc.dnd4e.controller._
  import vcc.dnd4e.model.TrackerContext
  var coord=vcc.controller.Coordinator.initialize(new TrackerController(new TrackerContext){
    addQueryHandler(new vcc.dnd4e.controller.TrackerQueryHandler(context))
    addPublisher(new vcc.dnd4e.controller.DefaultChangePublisher())
    val processor= new vcc.controller.TransactionalProcessor[TrackerContext](context) with TrackerEffectHandler with TrackerContextHandler with InitiativeActionHandler
    addPublisher(new vcc.dnd4e.controller.TrackerEffectPublisher(context))
  })

  var uia=new vcc.dnd4e.view.actor.UserInterface(coord.tracker)

  coord.start
  
  val commandPanel= new vcc.dnd4e.view.CombatantActionPanel(uia,coord.tracker)
  val seqTable = new vcc.dnd4e.view.SequenceTable(uia)
  val card=new vcc.dnd4e.view.CombatantCard(coord.tracker)
  val statusBar=new StatusBar(uia)
  
  // Register panel with UIA
  uia.addSequenceListener(seqTable)
  uia.addSequenceListener(commandPanel)
  uia.addContextListener(commandPanel)
  uia.addContextListener(seqTable)
  uia.addContextListener(card)
  uia.setStatusBar(statusBar)
  uia.start
  coord.addObserver(uia)

  
  def top = new MainFrame {
    title = "Virtual Combat Cards"
    preferredSize=new java.awt.Dimension(1000,600)
    contents= new BorderPanel {
      add(commandPanel,BorderPanel.Position.West)
      add(card,BorderPanel.Position.East)
      add(seqTable,BorderPanel.Position.Center)
      add(new MainMenu(coord,uia),BorderPanel.Position.North)
      add(statusBar,BorderPanel.Position.South)
    }
  }
  
}	