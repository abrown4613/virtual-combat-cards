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

import scala.swing._
import vcc.util.swing._
import java.awt.Window
import vcc.infra.ConfigurationFinder
import java.io.File
import vcc.infra.datastore.naming.DataStoreURI
import org.slf4j.LoggerFactory
import java.util.UUID

class ConfigurationDialog(owner: Window, initial: Boolean) extends ModalPromptDialog[Boolean](owner, "Virtual Combat Cards Configuration") {

  Configuration.dumpToLog()

  private val logger = LoggerFactory.getLogger("startup")

  private val homeDirRadioButton = new RadioButton("Use home directory")
  private val vccDirRadioButton = new RadioButton("Virtual Combat Cards directory")
  private val importSample = new CheckBox("Import sample data to you compendium")
  importSample.selected = true
  private val storeButtonGroup = new ButtonGroup(homeDirRadioButton, vccDirRadioButton)
  storeButtonGroup.select(homeDirRadioButton)

  private val startWebServerCheck = new CheckBox("Start VCC Web Server automatically")
  startWebServerCheck.selected = Configuration.autoStartWebServer.value

  private val logStore = new CheckBox("Keep logs of all executions (will require manual cleanup)")
  logStore.selected = Configuration.storeLogs.value

  private val collectMetricsCheckbox = new CheckBox("Anonymously collect information about your usage of VCC")
  collectMetricsCheckbox.selected = Configuration.metricIdentifier.value.isDefined

  private val userHome = new File(System.getProperties.getProperty("user.home"), "vcc")
  private val vccHome = new File(System.getProperties.getProperty("user.dir"), "userdata")

  private val modalPanel = new MigPanel("") {
    if (initial) {
      add(new Label("<html><body><p>Could not find your Virtual Combat Cards configuration, select from the options below to create a configuration.</p></body></html>"), "w 400, h 30, gapbottom 5, wrap")
      addSeparator("Your Compendium ")
      add(new Label("Please specify where you want to store you data:"), "wrap, gaptop rel, gapleft 10")
      add(homeDirRadioButton, "wrap,gapleft 10")
      add(new Label("Directory: " + userHome), "wrap,gapleft 35")
      add(vccDirRadioButton, "wrap,gapleft 10")
      add(new Label("Directory: " + vccHome), "wrap,gapleft 35")
      add(importSample, "wrap, gaptop rel, gapleft 10, gapbottom unrel")
      startWebServerCheck.selected = true
    }
    addSeparator("Runtime options ")
    add(startWebServerCheck, "wrap")
    add(logStore, "wrap")
    add(collectMetricsCheckbox, "wrap")
    add(new Button(okAction), "split 3, gaptop 15")
    if (!initial) add(new Button(cancelAction))
  }

  contents = modalPanel
  placeOnScreenCenter()

  def collectResult(): Option[Boolean] = {
    val dir = if (homeDirRadioButton.selected) userHome else vccHome
    val cFile = if (initial) new File(dir.getParentFile, ConfigurationFinder.configFilename)
    else ConfigurationFinder.locateFile()

    if (initial) {
      if (!Configuration.createDirectoryTree(dir)) {
        logger.error("Failed to create data structure")
        return None
      }
    }

    Configuration.baseDirectory.value = if (homeDirRadioButton.selected) userHome else new File("userdata")
    val uri = if (homeDirRadioButton.selected)
      DataStoreURI.fromStorageString("vcc-store:directory:file:$HOME/vcc/compendium")
    else
      DataStoreURI.fromStorageString("vcc-store:directory:file:userdata/compendium")
    logger.debug("Setting compendium to: {}", uri.toString())
    Configuration.compendiumStoreID.value = uri

    Configuration.autoStartWebServer.value = startWebServerCheck.selected
    Configuration.storeLogs.value = logStore.selected

    if(Configuration.metricIdentifier.value.isDefined != collectMetricsCheckbox.selected) {
      Configuration.metricIdentifier.value = if (collectMetricsCheckbox.selected) {
        Some(UUID.randomUUID())
      } else {
        None
      }
    }

    Configuration.save(cFile)
    Configuration.load(cFile)
    Some(importSample.selected)
  }
}