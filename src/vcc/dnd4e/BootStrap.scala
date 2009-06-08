//$Id$
package vcc.dnd4e

import vcc.util.UpdateManager

object BootStrap {
  
  def getPropertyAsInt(name:String,default:Int):Int = try {
    System.getProperties.getProperty(name).toInt
  } catch {
    case _ => default
  }

  val version=new UpdateManager.Version(0,95,0,null) 
}
