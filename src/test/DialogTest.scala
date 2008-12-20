package vcc

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
        diag.peer.show
        //diag.peer.setModalBlocked(null,true)
    }
  }
  
  import scala.actors.Actor.{loop,react}
  val echoer=scala.actors.Actor.actor{
    loop {
      react {
        case s => println("***Echoer: "+s)
      }
    }
  }
  var initDiag=new vcc.view.dialog.InitiativeDialog(echoer)
  initDiag.roster(List(
    new   ViewCombatant('A,"Aza",44,-1),
    new vcc.view.ViewCombatant('B,"Beta",43,3),
    new vcc.view.ViewCombatant('G,"Gamma",43,3),
    new vcc.view.ViewCombatant('D,"Delta",24,2),
    new vcc.view.ViewCombatant('F,"Fi",43,7)
  ))
  dialogs=List(
    new MyDialog,
    initDiag
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
