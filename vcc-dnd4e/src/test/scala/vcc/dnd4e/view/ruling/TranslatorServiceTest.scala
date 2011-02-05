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

import org.specs.SpecificationWithJUnit
import vcc.controller.{Decision, Ruling}
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common._

class TranslatorServiceTest extends SpecificationWithJUnit with Mockito {
  val eid = EffectID(CombatantID("A"), 0)
  val ts = new TranslatorService()
  "TranslatorService" should {
    "throw exception when it cannot translate" in {
      ts.promptForRuling(new Ruling() {
        protected def decisionValidator(decision: Decision[_]): Boolean = false
      }) must throwAn[Exception]
    }

    "provide a controller for simple save " in {
      val c = ts.promptForRuling(SaveEffectRuling(null, "bad things"))
      c mustNot beNull
      c.panelIdentity must_== RulingDialog.SimpleSavePanelIdentity
    }

    "provide a controller for save special" in {
      val c = ts.promptForRuling(SaveEffectSpecialRuling(null, "bad things"))
      c mustNot beNull
      c.panelIdentity must_== SaveOrChangeValuePanel.Identity
    }

    "provide a controller for save versus death" in {
      val c = ts.promptForRuling(SaveVersusDeathRuling(CombatantID("A")))
      c mustNot beNull
      c.panelIdentity must_== RulingDialog.SaveVersusDeathPanelIdentity
    }

    "provide a controller for ongoing damage" in {
      val c = ts.promptForRuling(RegenerateByRuling(eid, "regenerate 6", 6))
      c mustNot beNull
      c.panelIdentity must_== RegeneratePromptControllerDelegate.panelId
    }

    "provide a controller for regenerations" in {
      val c = ts.promptForRuling(OngoingDamageRuling(eid, "regenerate 6", 6))
      c mustNot beNull
      c.panelIdentity must_== OngoingPromptControllerDelegate.panelId
    }

  }
}