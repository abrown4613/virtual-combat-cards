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
//$Id$
package vcc.dnd4e.view.ruling

import org.mockito.Mockito._
import org.mockito.Matchers._
import swing.Frame
import vcc.controller.Ruling
import org.uispec4j.interception.{WindowHandler, WindowInterceptor}
import org.uispec4j.{Window, Trigger, UISpec4J, UISpecTestCase}
import vcc.infra.prompter.{EnumerationValuePanel, PromptController, RulingPromptController}
import vcc.dnd4e.domain.tracker.common.{SaveVersusDeathDecision, SaveEffectDecision}

class RulingDialogTest extends UISpecTestCase {

  UISpec4J.init()

  trait SomeRuling extends Ruling

  val frame = new Frame()
  val dialog = new RulingDialog(frame)
  val mController = mock(classOf[RulingPromptController[SomeRuling]])

  def testHaveSimpleSavePanel() {
    when(mController.panelIdentity).thenReturn(RulingDialog.SimpleSavePanelIdentity)
    openAndCancelDialog(mController).run()
    verify(mController).panelIdentity()
    verify(mController).decoratePanel(any[EnumerationValuePanel[SaveEffectDecision.type]])
  }

  def testHaveSaveSpecialPanel() {
    when(mController.panelIdentity).thenReturn(SaveOrChangeValuePanel.Identity)
    openAndCancelDialog(mController).run()
    verify(mController).panelIdentity()
    verify(mController).decoratePanel(any[SaveOrChangeValuePanel])
  }

  def testHaveSaveVsDeathPanel() {
    when(mController.panelIdentity).thenReturn(RulingDialog.SaveVersusDeathPanelIdentity)
    openAndCancelDialog(mController).run()
    verify(mController).panelIdentity()
    verify(mController).decoratePanel(any[EnumerationValuePanel[SaveVersusDeathDecision.type]])
  }

  private def openAndCancelDialog(controller: PromptController): WindowInterceptor = {
    WindowInterceptor.init(new Trigger() {
      def run {
        dialog.promptUser("lorem", List(controller))
      }
    }).process(new WindowHandler() {
      def process(window: Window): Trigger = {
        window.getButton("Cancel").triggerClick()
      }
    })
  }
}