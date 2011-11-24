/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.ruling

import javax.swing.{AbstractAction, JButton, JFrame}
import java.awt.event.ActionEvent
import org.uispec4j.interception.WindowInterceptor
import vcc.tracker.{Ruling, RulingContext}
import org.junit.Assert
import org.uispec4j._
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event.{StartCombatEvent, AddCombatantToOrderEvent, AddCombatantEvent}
import vcc.dnd4e.tracker.command.{EndRoundCommand, StartRoundCommand}
import vcc.dnd4e.tracker.ruling.SustainEffectRuling

class RulingDialogLayoutSample extends JFrame("Ruling Dialog Sample") {
  private var context: RulingContext[CombatState] = null
  private var dialogResult: Option[List[Ruling[CombatState, _, _]]] = null

  def setContext(context: RulingContext[CombatState]) {
    this.context = context
  }

  def getResult = dialogResult

  add(new JButton(new AbstractAction("Test") {
    def actionPerformed(e: ActionEvent) {
      val dialog = new RulingDialog(context, null)
      val result = dialog.promptUser()
      dialog.dispose()
      println("Result: " + result)
      dialogResult = dialog.dialogResult
    }
  }))
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

object RulingDialogLayoutSample extends SampleStateData {
  private val eid = EffectID(combA, 1)
  private val state = CombatState.empty.transitionWith(List(
    AddCombatantEvent(Some(combA), null, goblinEntity),
    AddCombatantEvent(Some(combB), null, pcEntity),
    AddCombatantToOrderEvent(InitiativeDefinition(combA, 0, List(15))),
    AddCombatantToOrderEvent(InitiativeDefinition(combB, 1, List(10))),
    StartCombatEvent
  ))


  def main(args: Array[String]) {
    val frame = new RulingDialogLayoutSample()
    frame.setContext(RulingContext(
      state,
      EndRoundCommand(ioiA0),
      List(SustainEffectRuling(eid, None))))
    frame.pack()
    frame.setVisible(true)
  }
}

class RulingDialogTest extends UISpecTestCase with WindowInterceptorWrapper with SampleStateData {
  private val state = CombatState.empty.transitionWith(List(
    AddCombatantEvent(Some(combA), null, goblinEntity),
    AddCombatantEvent(Some(combB), null, pcEntity),
    AddCombatantToOrderEvent(InitiativeDefinition(combA, 0, List(15))),
    AddCombatantToOrderEvent(InitiativeDefinition(combB, 1, List(10))),
    StartCombatEvent
  ))

  var dialogOwner: RulingDialogLayoutSample = null

  override def setUp() {
    super.setUp()

    dialogOwner = new RulingDialogLayoutSample
    dialogOwner.pack()

    setAdapter(new UISpecAdapter {
      def getMainWindow: Window = {
        new Window(dialogOwner)
      }
    })
  }

  def testOpenWindowEmpty_thenCancel() {
    dialogOwner.setContext(RulingContext(CombatState.empty, null, Nil))
    showRulingDialog ~> clickButton("Cancel") ~> run
    Assert.assertEquals(None, dialogOwner.getResult)
  }

  def testOpenWindowEmpty_thenOK() {
    dialogOwner.setContext(RulingContext(CombatState.empty, null, Nil))
    showRulingDialog ~> clickButton("OK") ~> run
    Assert.assertEquals(Some(Nil), dialogOwner.getResult)
  }

  def testTitleWhenDialog_isInStartRoundContext() {
    dialogOwner.setContext(RulingContext(state, StartRoundCommand(ioiA0), Nil))
    showRulingDialog ~> mustBeTrue(_.titleEquals("[Aº] Goblin - Start Round")) ~>
      mustBeTrue(_.titleContains("Start")) ~> clickButton("Cancel") ~> run
  }

  def testTitleWhenDialog_isInEndRoundContext() {
    dialogOwner.setContext(RulingContext(state, EndRoundCommand(ioiB0), Nil))
    showRulingDialog ~> mustBeTrue(_.titleEquals("[Bº] Fighter - End Round")) ~>
      clickButton("Cancel") ~> run
  }

  def testPromptForSustainEffectIsWellFormed() {
    val eid = EffectID(combA, 0)
    dialogOwner.setContext(RulingContext(state, EndRoundCommand(ioiB0), List(SustainEffectRuling(eid, None))))
    showRulingDialog ~> mustBeTrue(_.getRadioButton("Sustain").isEnabled) ~>
      mustBeTrue(_.getRadioButton("Cancel").isEnabled) ~>
      mustBeFalse(_.getButton("Ok").isEnabled) ~> clickButton("Cancel") ~> run
  }

  private def showRulingDialog = WindowInterceptor.init(getMainWindow.getButton("Test").triggerClick())
}