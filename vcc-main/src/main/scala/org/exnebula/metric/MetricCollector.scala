/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package org.exnebula.metric

import java.io.File
import java.awt.Toolkit
import vcc.util.DirectoryIterator
import vcc.dnd4e.compendium.Compendium
import vcc.dnd4e.BootStrap

class MetricCollector() {

  def collect(baseDirectory: File): Map[String, String] = {
    Map(
      "screensize" -> screenSize,
      "os.name" -> getOS,
      "vcc.version" -> BootStrap.version.versionString,
      "java.spec" -> System.getProperty("java.specification.version"),
      "compendium.captured" -> getCapturedCount(baseDirectory),
      "compendium.creatures" -> Compendium.activeRepository.getMonsterSummaries.size.toString,
      "compendium.characters" -> Compendium.activeRepository.getCharacterSummaries.size.toString,
      "compendium.traps" -> Compendium.activeRepository.getTrapSummaries.size.toString)
  }

  private def screenSize: String = {
    val dimension = Toolkit.getDefaultToolkit.getScreenSize
    "%dx%d".format(dimension.width, dimension.height)
  }

  private def getOS: String =
    System.getProperty("os.name")

  private def getCapturedCount(file: File): String = {
    val captureDir = new File(file, "dndicache")
    val dirIterator = new DirectoryIterator(captureDir, true)
    dirIterator.filter(_.getName.endsWith(".xml")).size.toString
  }

}