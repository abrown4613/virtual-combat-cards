//$Id$
package vcc

import scala.swing._
import scala.swing.event._ 
import vcc.util.swing._
import javax.swing.BorderFactory

import vcc.view.ViewCombatant
import scala.actors.Actor
import scala.actors.Actor.{actor,loop,react}
import view.{SequenceTable,ViewCombatant}
import vcc.view.dialog.FileChooserHelper
import vcc.controller._

class MainMenu(tracker:Actor,uia:vcc.view.actor.UserInterface) extends MenuBar {
  var fileMenu=new Menu("File");
  fileMenu.contents += new MenuItem(Action("Load Party"){
    var file=FileChooserHelper.chooseOpenFile(this.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      var l=vcc.model.PartyLoader.loadFromFile(file.get)
      var id=0
      for(x<-l)  { 
        tracker ! actions.AddCombatant(Symbol(if(x.id!=null)x.id else {id+=1; id.toString}),x)
      }
      tracker ! actions.Enumerate(uia)
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
    val diag=new vcc.view.dialog.InitiativeDialog(tracker)
    diag.visible=true
  })
  combatMenu.contents += new MenuItem(Action("End Combat") {
    tracker ! vcc.controller.actions.EndCombat()
    tracker ! actions.Enumerate(uia)
  })
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(Action("Clear Monsters"){
    tracker ! actions.ClearCombatants(false)
    tracker ! actions.Enumerate(uia)
  })
  combatMenu.contents +=new MenuItem(Action("Clear All"){
    tracker ! actions.ClearCombatants(true)
    tracker ! actions.Enumerate(uia)
  })

  var viewMenu= new Menu("View")
  var hideDeadMenu=new CheckMenuItem("Hide Dead")
  hideDeadMenu.action=Action("Hide Dead"){
    uia ! vcc.view.actor.SetOption('HIDEDEAD,hideDeadMenu.peer.isSelected)
  }
  viewMenu.contents +=hideDeadMenu
  viewMenu.contents += new Separator
  viewMenu.contents += new MenuItem(Action("Encounter/Party Editor..."){
    vcc.view.dialog.EncounterEditorDialog.visible=true
  })
  
  contents+=fileMenu
  contents+=combatMenu
  contents+=viewMenu
}

object Main extends SimpleGUIApplication {
  val log=scala.actors.Actor.actor {
    loop {
      react {
        case s=>println("Log: "+s)
      }
    }
  }
  val tracker=new vcc.controller.Tracker(log)
  var uia=new vcc.view.actor.UserInterface(tracker)
  tracker.setUserInterfaceActor(uia)
  
  val commandPanel= new vcc.view.CombatantActionPanel(tracker)
  var seqTable = new SequenceTable(uia)
  
  
  def top = new MainFrame {
    title = "Virtual Combat Cards"
    contents= new BorderPanel {
      add(commandPanel,BorderPanel.Position.East)
      add(seqTable,BorderPanel.Position.Center)
      add(new MainMenu(tracker,uia),BorderPanel.Position.North)
    }
  }
  
  // Register panel with UIA
  uia.addSequenceListener(seqTable)
  uia.addSequenceListener(commandPanel)
  uia.addContextListner(commandPanel)
  uia.addContextListner(seqTable)

  // Start actors
  tracker.start
  uia.start
}	