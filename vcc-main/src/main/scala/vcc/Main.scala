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
package vcc

import vcc.infra.startup._
import vcc.infra.LogService
import vcc.util.swing.SwingHelper
import javax.swing.JOptionPane
import org.slf4j.{Logger, LoggerFactory}
import dnd4e.{ConfigurationDialog, BootStrap}
import java.io.File

object Main {

  def main(args: Array[String]) {

    LogService.initializeStartupLog()

    val logger = LoggerFactory.getLogger("startup")

    assertWriteOnJarDirectory(classOf[ConfigurationDialog], logger)
    val frame = SplashWindow.showSplash(BootStrap)
    if (frame != null) {
      SwingHelper.invokeLater {
        frame.visible = true
      }
      logger.info("Initialization complete.")
    } else {
      warnFailure(SplashWindow)
      logger.error("Initialization failed.")
      sys.exit(1)
    }
  }

  def warnFailure(win: java.awt.Window) {
    JOptionPane.showMessageDialog(win, "Virtual Combat Cards initialization failed. Please check launch.log for clues.\n" +
      "Check http://www.exnebula.org/vcc to look for help or report an issue.",
      "Initialization failed", JOptionPane.ERROR_MESSAGE)
  }

  def assertWriteOnJarDirectory(clazz: java.lang.Class[_], logger: Logger) {
    val jarFile = clazz.getProtectionDomain.getCodeSource.getLocation.getFile.replaceAll("%20", " ")
    if (!new File(jarFile).getParentFile.canWrite) {
      logger.error("Cannot write to parent of {}", jarFile)
      JOptionPane.showMessageDialog(null,
        """
          |Virtual Combat Cards must be able to write into the application folder.
          |
          |If you are running on a Mac please make sure you copy the application to some other
          |folder. Running from the DMG is not possible since it prevents updates.
        """.stripMargin,
        "Application Folder is Read Only", JOptionPane.ERROR_MESSAGE)
      sys.exit(1)
    }
  }
}