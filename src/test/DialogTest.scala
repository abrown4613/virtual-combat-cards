package test

import swing._
import swing.event._

import vcc.util.swing._
import vcc.view._

class MyDialog extends DialogFrame {
  title="My diag"
  var v=0
  val btn=new Button("Press me")
  var label= new Label("Push me")
  contents=new BoxPanel(Orientation.Vertical) {
    contents+=btn
    contents+=label
  }
  minimumSize=new java.awt.Dimension(200,100)
  listenTo(btn)
  reactions +={
    case ButtonClicked(this.btn) => v+=1; label.text="Push again, I've been pushed "+v+" times"
    case vcc.util.swing.DialogClosed(s,cancel)=>println("Dialog Closed "+s+" canceled "+cancel)
  }
}

object DialogTest extends SimpleGUIApplication {
  
  var buttons:List[Publisher]=List()
  var dialogs:List[Frame]=List()
  
  var top=new MainFrame {
    title="Dialog Testing"
    minimumSize=new java.awt.Dimension(400,200)
    contents=new FlowPanel {
      contents+=new Button("MyDialog")
      //buttons=contents
    }	
    reactions += {
      case ButtonClicked(s) => 
        println(s); 
        var diag=map(s.text); 
        diag.pack
        diag.visible=true
    }
  }
  
  import scala.actors.Actor.{loop,react,reply}
  val echoer=scala.actors.Actor.actor{
    var l=List(
      new vcc.model.TrackerCombatant('A,"Aza",44,-1,vcc.model.CombatantType.Monster),
      new vcc.model.TrackerCombatant('B,"Beta",43,3,vcc.model.CombatantType.Monster),
      new vcc.model.TrackerCombatant('G,"Gamma",43,3,vcc.model.CombatantType.Monster),
      new vcc.model.TrackerCombatant('D,"Delta",24,2,vcc.model.CombatantType.Monster),
      new vcc.model.TrackerCombatant('F,"Fi",43,7,vcc.model.CombatantType.Monster)
    )
    loop {
      react {
        case vcc.model.actions.QueryCombatantMap(func) =>
          println("Requested list of chars")
          reply(l.map(func))
        case s => println("***Echoer: "+s)
      }
    }
  }
  var initDiag=new vcc.view.dialog.InitiativeDialog(echoer)
  dialogs=List(
    new MyDialog,
    initDiag,
    vcc.view.dialog.EncounterEditorDialog
  )
  var btns= dialogs map (x=>new Button(x.getClass.getCanonicalName))
  var map=collection.mutable.Map.empty[String,Frame]
  for(x <- dialogs) { map+=x.getClass.getCanonicalName->x}
  for(x <- btns) {top.listenTo(x)}
  top.contents= new FlowPanel {
    contents++btns
  }
  println(map)
}
