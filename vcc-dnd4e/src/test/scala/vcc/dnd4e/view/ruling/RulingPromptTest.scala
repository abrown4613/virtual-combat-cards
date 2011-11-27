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

import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j.{Window, Trigger, UISpecTestCase}
import vcc.dnd4e.tracker.event.{AddEffectEvent, StartCombatEvent, AddCombatantToOrderEvent, AddCombatantEvent}
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.command.{EndRoundCommand, StartRoundCommand}
import javax.swing.JLabel
import org.uispec4j.assertion.Assertion
import junit.framework.Assert
import vcc.dnd4e.tracker.ruling.{SustainEffectRulingResult, SustainEffectRuling}
import org.uispec4j.finder.ComponentFinder
import vcc.dnd4e.tracker.common.{EffectID, Duration, InitiativeDefinition, CombatState}
import vcc.tracker.{Command, Ruling, RulingContext}

class RulingPromptTest extends UISpecTestCase with SampleStateData {
  private val effectNameOnA = "something bad on A"
  private val effectNameOnB = "something bad on B"
  private val nameCombA = "[Aº] Goblin"
  private val nameCombB = "[Bº] Fighter"

  private val state = CombatState.empty.transitionWith(List(
    AddCombatantEvent(Some(combA), null, goblinEntity),
    AddCombatantEvent(Some(combB), null, pcEntity),
    AddCombatantToOrderEvent(InitiativeDefinition(combA, 0, List(15))),
    AddCombatantToOrderEvent(InitiativeDefinition(combB, 1, List(10))),
    StartCombatEvent,
    AddEffectEvent(combA, combB, Condition.Generic(effectNameOnA, false), Duration.EndOfEncounter),
    AddEffectEvent(combB, combA, Condition.Generic(effectNameOnB, false), Duration.EndOfEncounter)
  ))

  override def setUp() {
    super.setUp()
  }

  private def createAndShowDialog(rulingContext: RulingContext[CombatState]): WindowInterceptor = {
    WindowInterceptor.init(new Trigger {
      def run() {
        RulingPrompt.promptUser(rulingContext)
      }
    })
  }

  def testStartRoundTitle() {
    val context = RulingContext(state, StartRoundCommand(ioiA0), Nil)
    createAndShowDialog(context).process(validateTitleAndCancel(nameCombA + " - Start Round")).run()
  }

  def testEndRoundTitle() {
    val context = RulingContext(state, EndRoundCommand(ioiB0), Nil)
    createAndShowDialog(context).process(validateTitleAndCancel(nameCombB + " - End Round")).run()
  }

  def windowContainsLabel(window: Window, expected: String): Assertion = {
    new Assertion {
      def check() {
        val finder = new ComponentFinder(window.getAwtContainer)
        finder.getComponent(expected, Array(classOf[JLabel]), "JLabel")
      }
    }
  }

  case class dialogController(context: RulingContext[CombatState]) {
    private var ret: List[Ruling[CombatState, _, _]] = Nil

    def showDialogAndProcess(expectedTitle: String, button: String) {
      WindowInterceptor.init(new Trigger {
        def run() {
          ret = RulingPrompt.promptUser(context)
        }
      }).process(new WindowHandler() {
        def process(window: Window): Trigger = {
          assertTrue(windowContainsLabel(window, expectedTitle))
          window.getRadioButton(button).click()
          window.getButton("Ok").triggerClick()
        }
      }).run()
    }

    def collectAnswer = ret
  }

  private val commandEndRoundB = EndRoundCommand(ioiB0)
  private val commandEndRoundA = EndRoundCommand(ioiA0)

  def testSustainEffect() {
    val controller = dialogController(makeContext(commandEndRoundB, makeSustainEffectList(eidA1)))
    val expectedTitle = nameCombB + " - Sustain effect: " + effectNameOnA
    controller.showDialogAndProcess(expectedTitle, "Sustain")
    Assert.assertEquals(List(SustainEffectRuling(eidA1, Some(SustainEffectRulingResult.Sustain))), controller.collectAnswer)
  }

  def testNotSustainEffect() {
    val controller = dialogController(makeContext(commandEndRoundA, makeSustainEffectList(eidB1)))
    val expectedTitle = nameCombA + " - Sustain effect: " + effectNameOnB
    controller.showDialogAndProcess(expectedTitle, "Cancel")
    Assert.assertEquals(List(SustainEffectRuling(eidB1, Some(SustainEffectRulingResult.Cancel))), controller.collectAnswer)
  }

  private def makeContext(command: Command[CombatState], rulings: List[Ruling[CombatState, _, _]]) = {
    RulingContext(state, command, rulings)
  }

  private def makeSustainEffectList(id: EffectID): List[Ruling[CombatState, _, _]] = {
    List(SustainEffectRuling(id, None))
  }

  private def validateTitleAndCancel(title: String): WindowHandler = {
    new WindowHandler() {
      def process(window: Window): Trigger = {
        assertTrue(window.titleEquals(title))
        window.getButton("Cancel").triggerClick()
      }
    }
  }
}