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

class MainMenu(tracker:Actor,uia:vcc.view.actor.UserInterface) extends MenuBar {
  var fileMenu=new Menu("File");
  fileMenu.contents += new MenuItem(Action("Load Party"){
    val fileOpen= new FileChooser(new java.io.File(System.getProperty("user.dir")))
    fileOpen.fileFilter=new javax.swing.filechooser.FileNameExtensionFilter("XML Files","xml")
    val result=fileOpen.showOpenDialog(this)
    if(result==FileChooser.Result.Approve) {
      println("Load party: "+fileOpen.selectedFile)
      var l=vcc.model.PartyLoader.loadFromFile(fileOpen.selectedFile)
      var id=0
      for(x<-l)  { 
        tracker ! vcc.model.actions.AddCombatant(Symbol(if(x.id!=null)x.id else {id+=1; id.toString}),x)
      }
      tracker ! vcc.model.actions.Enumerate(uia)
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
    diag.roster(uia.sequence) 
    diag.visible=true
  })
  combatMenu.contents += new MenuItem(Action("End Combat") {
    tracker ! vcc.model.actions.EndCombat()
    tracker ! vcc.model.actions.Enumerate(uia)
  })
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(Action("Clear Monsters"){
    tracker ! vcc.model.actions.ClearCombatants(false)
    tracker ! vcc.model.actions.Enumerate(uia)
  })
  combatMenu.contents +=new MenuItem(Action("Clear All"){
    tracker ! vcc.model.actions.ClearCombatants(true)
    tracker ! vcc.model.actions.Enumerate(uia)
  })
  
  contents+=fileMenu
  contents+=combatMenu
}

object Main extends SimpleGUIApplication {
  val log=scala.actors.Actor.actor {
    loop {
      react {
        case s=>println("Log: "+s)
      }
    }
  }
  val tracker=new vcc.model.Tracker(log)
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