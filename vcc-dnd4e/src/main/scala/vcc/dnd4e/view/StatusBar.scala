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
package vcc.dnd4e.view

import vcc.util.swing.MigPanel
import scala.swing._
import javax.swing.Timer
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.Font
import vcc.updater.UpdateManager
import UpdateManager.Version

class StatusBar(currentVersion: Version) extends MigPanel("ins 1, fillx", "[][][grow][][]", "[]") {

  // Simple class for separator
  private class StatusBarSeparator() extends Component {
    override lazy val peer = new javax.swing.JSeparator(javax.swing.SwingConstants.VERTICAL)
  }

  private val separatorLayout = "growy, w 3!, gp 0, growx 0"
  private val versionLabel = new Label("Version: " + currentVersion.versionString)
  private val baseFont = new Font(versionLabel.font.getFamily, Font.PLAIN, versionLabel.font.getSize)

  versionLabel.font = baseFont
  versionLabel.tooltip = "Version of Virtual Combat Cards"

  private val tipLabel = new Label("Welcome to Virtual Combat Cards")
  tipLabel.xAlignment = Alignment.Left
  tipLabel.font = baseFont

  private val memoryLabel = new Label("Memory")
  memoryLabel.font = baseFont
  memoryLabel.tooltip = "Memory used by Virtual Combat Cards"
  border = javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED)

  add(versionLabel, "align left")
  add(new StatusBarSeparator, separatorLayout)
  add(tipLabel, "align left, growx 100")
  add(new StatusBarSeparator, separatorLayout)
  add(memoryLabel, "w 100!, gp 0, growx 0")
  startUsageTimer()

  /**
   * Set the text in the tip area
   * @param str Text to be set
   */
  def setTipText(str: String) {
    tipLabel.text = str
  }

  //Start the menu bar update for memory usage
  private def startUsageTimer() {
    val timer = new Timer(15 * 1000, new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        updateUsage()
      }
    })
    timer.start()
    updateUsage()
  }

  private def updateUsage() {
    def inMB(v: Long) = (v / (1024 * 1024)).formatted("%dM")
    val free = Runtime.getRuntime.freeMemory
    val total = Runtime.getRuntime.totalMemory
    memoryLabel.text = inMB(total - free) + " of " + inMB(total)
  }

}
