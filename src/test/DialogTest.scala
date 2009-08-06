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
//$Id$
package test

import swing._
import swing.event._

import vcc.util.swing._
import vcc.dnd4e.view._
import vcc.dnd4e.model.TrackerCombatant

import vcc.dnd4e.model.CombatantType

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
      new TrackerCombatant('A,null,"Aza",44,-1,CombatantType.Monster,null),
      new TrackerCombatant('B,null,"Beta",43,3,CombatantType.Monster,null),
      new TrackerCombatant('G,null,"Gamma",43,3,CombatantType.Monster,null),
      new TrackerCombatant('D,null,"Delta",24,2,CombatantType.Monster,null),
      new TrackerCombatant('F,null,"Fi",43,7,CombatantType.Monster,null)
    )
    loop {
      react {
        case vcc.dnd4e.controller.actions.QueryCombatantMap(func) =>
          println("Requested list of chars")
          reply(l.map(func))
        case s => println("***Echoer: "+s)
      }
    }
  }
  var initDiag=new vcc.dnd4e.view.dialog.InitiativeDialog(echoer)
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
