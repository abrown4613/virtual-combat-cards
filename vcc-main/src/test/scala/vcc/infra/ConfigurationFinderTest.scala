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
package vcc.infra

import org.specs2.SpecificationWithJUnit
import java.io.File

class ConfigurationFinderTest extends SpecificationWithJUnit {
  private val slash = File.separator
  private val configName = "vcc.properties"
  val macPath = System.getProperty("user.home") + slash + "Library" + slash + "Preferences" + slash + configName

  def is = sequential ^ "ConfigurationFinder".title ^
    "must scan only path with defined properties" ! visitAllPathWithDefinedVCCHome ^
    "must scan proper directories" ! visitAllPaths ^
    findAllFiles() ^
    end

  def visitAllPaths = {
    System.setProperty("vcc.home", new File(slash + "somedir").getAbsolutePath)
    var visited: List[String] = Nil
    ConfigurationFinder.locateFileInternal(file => {
      visited = file.getAbsolutePath :: visited
      false
    })
    visited.reverse must_== makeSearchPaths()
  }

  def visitAllPathWithDefinedVCCHome = {
    System.clearProperty("vcc.home")
    var visited: List[String] = Nil
    ConfigurationFinder.locateFileInternal(file => {
      visited = file.getAbsolutePath :: visited
      false
    })
    visited.reverse must_== makeSearchPaths()
  }

  def findAllFiles() = {
    for (filename <- makeSearchPaths()) yield {
      "find " + filename ! findFile(filename)
    }
  }

  def findFile(fileExpected: String) = {
    val matcher: File => Boolean = file => file.getAbsolutePath == fileExpected
    ConfigurationFinder.locateFileInternal(matcher) must_== Some(new File(fileExpected))
  }

  private def makeSearchPaths(): List[String] = {
    List("vcc.home", "user.dir", "user.home").filter(System.getProperty(_) != null).
      map(System.getProperty(_) + slash + configName).toList ++ List(macPath)
  }
}