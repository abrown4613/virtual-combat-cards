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
package vcc.util

import java.io.File
import java.util.Properties

/**
 * This is a helper class that is responsible for checking errors
 * and providing default values to the configuration when loading
 */
abstract class ConfigurationLoader {
  def load(file:File):Properties
}

object Configuration {
  
  private val searchVars= List("vcc.home","user.dir","user.home")
  
  private val filename="vcc.properties"

  private var _properties:Properties = null
  
  private def getFile():java.io.File = {
	for(ev<-searchVars) {
	  val prop=System.getProperty(ev)
	  if(prop!=null) {
		  var file=new File(prop,filename)
		  if(file.exists && file.isFile && file.canRead) return file
	  }
	}
	null
  }
  
  def isConfigured = getFile != null
  
  /**
   * Load and validade a configuration file
   */
  def load(cloader:ConfigurationLoader) {
    if(_properties!=null) {
      val file=getFile()
      if(file!=null) _properties=cloader.load(file)
    }
  }
  
  def properties:Properties = if(_properties!=null) _properties 
                              else throw new Exception("Configuration has not been loaded!")
}
