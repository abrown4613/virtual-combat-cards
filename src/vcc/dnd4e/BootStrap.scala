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

object BootStrap {
  
  def getPropertyAsInt(name:String,default:Int):Int = try {
    System.getProperties.getProperty(name).toInt
  } catch {
    case _ => default
  }

  val version=new UpdateManager.Version(0,99,0,"RC1") 
  
  def initialize() {
    if(!vcc.util.Configuration.isConfigured) {
      println("Can't find the configuration")
    }
    //FIXME This is just for testing
    val sampleCompendiumID = vcc.model.datastore.DataStoreURI.directoryEntityStoreIDFromFile(new java.io.File("sample-compendium"))
    Registry.register(sampleCompendiumID, vcc.model.datastore.EntityStoreFactory.createStore(sampleCompendiumID))
    Registry.register("Compendium",sampleCompendiumID)
    Registry.register("SampleCompendium",sampleCompendiumID)

    // Compendium depends on active Compendium settings
    vcc.dnd4e.model.Compendium.initialize

  }
}
