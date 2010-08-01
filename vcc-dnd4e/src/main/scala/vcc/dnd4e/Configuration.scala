/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

package vcc.dnd4e

import vcc.infra._
import vcc.infra.startup.StartupStep

import java.io.File
import vcc.infra.datastore.naming.DataStoreURI
import vcc.infra.datastore.DataStoreFactory

object Configuration extends AbstractConfiguration with StartupStep {
 
  val autoStartWebServer = Property[Boolean]("vcc.dnd4e.autoWebServer","true", x => x == "true")
  val storeLogs = Property[Boolean]("vcc.dnd4e.storeLogs","false", x => x == "true")
  val baseDirectory = Property[File]("vcc.dnd4e.basedir",System.getProperty("user.dir"), x => {new File(x)})
  val compendiumStoreID = Property[DataStoreURI]("vcc.dnd4e.compendium",null, x => {
    if(x != null) {
      expandDataStoreURI(DataStoreURI.fromStorageString(x))
    } else null
  })
  
  def expandDataStoreURI(baseEsid:DataStoreURI):DataStoreURI = {
    if(baseEsid == null) return null
	val dsb = DataStoreFactory.getDataStoreBuilder(baseEsid)
	if(dsb != null) {
	  if(dsb.isResolvedDataStoreURI(baseEsid)) 
		baseEsid
	  else {
		logger.info("Configuration: Will expand ESID: {}",baseEsid)
		val modEsid = dsb.resolveDataStoreURI(baseEsid,Map("HOME"->new File(System.getProperty("user.home")).toURI,"BASE"->baseDirectory.value.toURI))
		logger.info("Configuration: Expanded to: {}",modEsid)
		modEsid
	  }
	} else 
	  null
  }
  
  def createDirectoryTree(baseDir:java.io.File):Boolean = {
    if(baseDir.exists && ! baseDir.isDirectory ) {
      logger.error("{} exists but is a file, cannot complete configuration",baseDir)
      return false 
    }
    if(!baseDir.exists) {
      logger.info("Have to create: {}",baseDir)
      if(!baseDir.mkdirs) {
        logger.warn("Failed to create directory: {}",baseDir)
        return false 
      }
    }
    // Got hear means directory is ok
    true
  }
  
  def isStartupComplete() = compendiumStoreID.value != null
}

import scala.swing._
import vcc.util.swing._
import java.awt.Window

class ConfigurationDialog(owner:Window,initial:Boolean) extends ModalDialog[Boolean](owner,"Virtual Combat Cards Configuration") {
  
  Configuration.dumpToLog()
  
  private val logger = org.slf4j.LoggerFactory.getLogger("startup")  

  private val homeDirRadioButton = new RadioButton("Use home directory")
  private val vccDirRadioButton = new RadioButton("Virtual Combat Cards directory")
  private val importSample = new CheckBox("Import sample data to you compendium")
  importSample.selected = true
  private val storeButtonGroup = new ButtonGroup(homeDirRadioButton,vccDirRadioButton)
  storeButtonGroup.select(homeDirRadioButton)

  private val startWebServerCheck = new CheckBox("Start VCC Web Server automatically")
  startWebServerCheck.selected = Configuration.autoStartWebServer.value
  
  private val logStore = new CheckBox("Keep logs of all executions (will require manual cleanup)")
  logStore.selected = Configuration.storeLogs.value

  private val userHome = new java.io.File(System.getProperties.getProperty("user.home"),"vcc")
  private val vccHome = new java.io.File(System.getProperties.getProperty("user.dir"),"userdata")
  
  private val mpanel =new MigPanel("") {
    if(initial) {
      add(new Label("<html><body><p>Could not find your Virtual Combat Cards configuration, select from the options below to create a configuration.</p></body></html>"),"w 400, h 30, gapbottom 5, wrap")
      addSeparator("Your Compendium ")
      add(new Label("Please specify where you want to store you data:"),"wrap, gaptop rel, gapleft 10")
      add(homeDirRadioButton,"wrap,gapleft 10")
      add(new Label("Directory: "+userHome),"wrap,gapleft 35")
      add(vccDirRadioButton,"wrap,gapleft 10")
      add(new Label("Directory: "+vccHome),"wrap,gapleft 35")
      add(importSample,"wrap, gaptop rel, gapleft 10, gapbottom unrel")
      startWebServerCheck.selected = true
    } 
    addSeparator("Runtime options ")
    add(startWebServerCheck,"wrap")
    add(logStore,"wrap")
    add(new Button(okAction), "split 3, gaptop 15")
    if(!initial) add(new Button(cancelAction))
  }
  
  contents = mpanel
  placeOnScreenCenter()
  
  def processOK() {
    val dir = if(homeDirRadioButton.selected) userHome else vccHome
    val cFile = if(initial) new File(dir.getParentFile,ConfigurationFinder.configFilename)
    			else ConfigurationFinder.locateFile
    
    if(initial) {
      if(!Configuration.createDirectoryTree(dir)) {
        logger.error("Failed to create data structure")
        return
      }
    }
    
    Configuration.baseDirectory.value = if(homeDirRadioButton.selected) userHome else new File("userdata")
    val uri = if(homeDirRadioButton.selected)
    	DataStoreURI.fromStorageString("vcc-store:directory:file:$HOME/vcc/compendium")
      else
    	DataStoreURI.fromStorageString("vcc-store:directory:file:userdata/compendium")
    logger.debug("Setting compendium to: {}",uri.toString)
    Configuration.compendiumStoreID.value = uri
    
    Configuration.autoStartWebServer.value = startWebServerCheck.selected
    Configuration.storeLogs.value = logStore.selected
    Configuration.save(cFile)
    Configuration.load(cFile)
    dialogResult = Some(importSample.selected)
  }
}