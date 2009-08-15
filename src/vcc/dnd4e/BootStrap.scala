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

object BootStrap extends StartupRoutine {
  
  def getPropertyAsInt(name:String,default:Int):Int = try {
    System.getProperties.getProperty(name).toInt
  } catch {
    case _ => default
  }

  val version=new UpdateManager.Version(0,99,0,"RC1") 

  def start(srw: StartupReportWindow):scala.swing.Frame = {
    try {
	  if(!vcc.util.Configuration.isConfigured) {
	    println("Can't find the configuration")
	  }
      callStartupStep(srw,"Logging") {
        import vcc.infra.LogService
        LogService.initializeLog("org.mortbay.log",LogService.level.Info)
        LogService.initializeLog("infra",LogService.level.Debug)
        LogService.initializeLog("domain",LogService.level.Debug)
        LogService.initializeLog("app",LogService.level.Debug)
        LogService.initializeLog("user",LogService.level.Info)
        LogService
      }	
      // Compendium depends on active Compendium settings
      callStartupStep(srw,"Compendium") {
        Compendium
        //FIXME This is just for testing
        val sampleCompendiumID = vcc.model.datastore.DataStoreURI.directoryEntityStoreIDFromFile(new java.io.File("sample-compendium"))
        val sampleCompendium = vcc.model.datastore.EntityStoreFactory.openStore(sampleCompendiumID)
        if(sampleCompendium == null) {
        	println("Failed to load compendium... exiting")
        	exit
        }	
        Registry.register(sampleCompendiumID, sampleCompendium)
        Registry.register("Compendium",sampleCompendiumID)
        Registry.register("SampleCompendium",sampleCompendiumID)
        Compendium.setActiveRepository(sampleCompendium)
        Compendium
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
    } catch {
      case s =>
        s.printStackTrace()
        null
    }
  }
}
