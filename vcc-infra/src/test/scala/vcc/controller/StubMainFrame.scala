/*
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.controller

import scala.swing._
import actors.Actor.{actor, receive}
import vcc.util.swing.{SwingHelper, ModalDialog}
import actors.Actor


class MyDialog(owner: Frame) extends ModalDialog[Boolean](owner, "Hello") {
  minimumSize = new Dimension(300, 200)
  val yesButton = new Button(Action("Yes") {
    dialogResult = Some(true)
    okAction()
  })
  val noButton = new Button(Action("No") {
    dialogResult = Some(false)
    okAction()
  })
  contents = new FlowPanel {
    contents += yesButton
    contents += noButton
    contents += new Button(cancelAction)
  }

  override def processOK() {

  }
}


case class ExecuteMessage(verb: Symbol, source: CommandSource)

class StubMainFrame(action: CommandSource => Unit) extends MainFrame with CommandSource {
  title = "Stub Frame"
  minimumSize = new Dimension(300, 300)

  private val innerActor = new Actor {
    def act()  {
       while (true) {
      receive{
        case ExecuteMessage(verb, source) =>
          action(source)
      }
       }
    }
  }
  innerActor.start()

  def provideDecisionsForRulings(rulings: List[Ruling[_]]): List[Decision[_]] = {
    val diag = new MyDialog(this)
    diag.visible = true
    result = diag.dialogResult
    resultLabel.text = "Complete"
    Nil
  }

  def actionCompleted(msg: String, producedChanges: Boolean) {
    setStatus("Complete")
  }

  def actionCancelled(reason: String) {
    setStatus("Cancelled")
  }

  private def setStatus(msg: String) {
    SwingHelper.invokeInEventDispatchThread{
      resultLabel.text = msg
    }
  }

  var result: Option[Boolean] = None

  private val actionButton = new Button(Action("Action") {
    if (action != null) {
      innerActor ! ExecuteMessage('EXECUTE, this)
    } else {
      resultLabel.text = "No action defined"
    }
  })
  actionButton.name = "ActionButton"

  private val resultLabel = new Label("Waiting")
  resultLabel.name = "ResultLabel"

  contents = new FlowPanel {
    contents += actionButton
    contents += resultLabel
  }

  visible = true

}

object RunStubMainFrame {
  def main(args: Array[String]) {
    val frame = new StubMainFrame(source => source.provideDecisionsForRulings(Nil))
    frame.visible = true
  }
}
