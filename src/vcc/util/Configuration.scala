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
