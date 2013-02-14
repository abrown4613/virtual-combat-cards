/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e

import compendium.CaptureTemplateEngine
import vcc.util.swing.XHTMLPaneAgent
import org.specs2.mutable.SpecificationWithJUnit
import vcc.infra.LogService

/**
 * Class to test singleton objects and resources
 */
class BootstrapTest extends SpecificationWithJUnit {
  LogService.initializeStartupLog()
  "BootStrap" should {
    "Load XHTMLPaneAgent configured properly" in {
      XHTMLPaneAgent.createInstance(BootStrap.getWebApplicationDirectory)
      (XHTMLPaneAgent.getInstance() must not beNull)
    }

    "Initialize CaptureTemplateEngine" in {
      CaptureTemplateEngine.initialize(BootStrap.getWebApplicationDirectory)
      (CaptureTemplateEngine.getInstance must not beNull) and
        (CaptureTemplateEngine.getInstance.fetchClassTemplate("monster") must not beNull)
    }

    "Verify Info.plist is bundled" in {
      val resource = this.getClass.getClassLoader.getResource("external/Info.plist")
      (resource must not beNull)
    }
  }
}