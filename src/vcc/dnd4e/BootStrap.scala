/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

import vcc.util.UpdateManager
import vcc.model.Registry
import vcc.dnd4e.model.Compendium

import vcc.infra.startup._
import vcc.infra.ConfigurationFinder
import vcc.infra.LogService

object BootStrap extends StartupRoutine {
  
  val logger = org.slf4j.LoggerFactory.getLogger("startup")
  
  def getPropertyAsInt(name:String,default:Int):Int = try {
    System.getProperty(name).toInt
  } catch {
    case _ => default
  }

  val version=new UpdateManager.Version(0,99,0,"RC1") 

  def start(srw: StartupReportWindow):scala.swing.Frame = {
	var createCompendium = false
	var importSampleCompendium = false
	logger.info("Starting VCC DND4E components...")

	callStartupStep(srw,"Loading configuration") {
	    if(!ConfigurationFinder.foundConfiguration) {
	      logger.info("Can't find the configuration, will configure")
	      val cdiag = new ConfigurationDialog(srw.ownerWindow,true)
	      cdiag.visible = true 
	      val result = cdiag.dialogResult
	      println(cdiag.dialogResult)
	      if(result == None) {
	        logger.warn("Failed to complete configuration, will exit")
	        return null
	      } else {
	    	createCompendium = true
	    	importSampleCompendium = result.get
	    	logger.debug("Create compedium requested, import sample? "+importSampleCompendium)
	      }
	    }
	    Configuration.load(ConfigurationFinder.locateFile)
	    Configuration
	}
	callStartupStep(srw,"Logging") {
	  val logs = Seq("org.mortbay.log","domain","app","infra","user")
	  val file = new java.io.File(Configuration.baseDirectory.value,"vcc.log")
	  logger.info("Starting to logging operations to: {}",file)
	  LogService.initializeLog(logs,file.toString,LogService.level.Debug,Configuration.storeLogs.value) 
	  LogService
	}	
    // Compendium depends on active Compendium settings
    callStartupStep(srw,"Compendium components") {
        Compendium
    }
      
    if(createCompendium) {
    	callStartupSimpleBlock(srw,"Create user compendium") {
          import vcc.model.datastore.EntityStoreFactory
    	  if(EntityStoreFactory.exists(Configuration.compendiumStoreID.value)) {
    	    logger.warn("Compendium exists, will assume it is valid and attempt to load")
    	    true
    	  } else {
    	    val es = EntityStoreFactory.createStore(Configuration.compendiumStoreID.value)
    	    if(es == null) {
    	      logger.error("Failed to create compendium {}",Configuration.compendiumStoreID.value)
    	      false
    	    } else {
    	      //TODO Import
    	      true
    	    } 
    	  }
    	}
    }
    callStartupSimpleBlock(srw,"Load Compendium") {
        import vcc.model.datastore.{EntityStoreID,EntityStore}
    	val compendiumID = Configuration.compendiumStoreID.value
    	logger.info("Opening compendium: {}",compendiumID)
        val compendium = vcc.model.datastore.EntityStoreFactory.openStore(compendiumID)
        if(compendium == null) {
          logger.warn("Failed to load compendium {}, will exit",compendiumID)
        } else {
          logger.info("Opened compendium {}",compendiumID)
          Registry.register("Compendium",compendiumID)
          Registry.register(compendiumID,compendium)
          Compendium.setActiveRepository(compendium)
        }	
        Registry.get[EntityStoreID]("Compendium").isDefined && Registry.get[EntityStore](Registry.get[EntityStoreID]("Compendium").get).isDefined 
    }
      
    callStartupStep(srw,"Web Server") {
        import vcc.infra.webserver.WebServer
        WebServer.initialize("webserver",4143)
    	Registry.get[WebServer]("webserver").get
    }
      
    callStartupStep(srw,"Core Tracker") {
    	import vcc.controller._
    	import vcc.dnd4e.controller._
    	import vcc.dnd4e.model.TrackerContext

    	val t = Tracker.initialize(new TrackerController(new TrackerContext){
    	  addQueryHandler(new vcc.dnd4e.controller.TrackerQueryHandler(context))
    	  addPublisher(new vcc.dnd4e.controller.DefaultChangePublisher())
    	  val processor= new vcc.controller.TransactionalProcessor[TrackerContext](context) with TrackerEffectHandler with TrackerContextHandler with InitiativeActionHandler
    	  addPublisher(new vcc.dnd4e.controller.TrackerEffectPublisher(context))
    	})
    	//Make sure it got registered
    	Registry.get[scala.actors.Actor]("tracker").get.asInstanceOf[StartupStep]
    }
      
    callStartupStep(srw,"XHTMLRenderer") {
        vcc.util.swing.XHTMLPane
    }
    
    srw.reportProgress(this,"Initialization complete.")
    new vcc.dnd4e.view.MasterFrame()
  }
}
