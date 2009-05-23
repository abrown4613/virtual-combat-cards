//$Id$
package vcc.dnd4e

object BootStrap {
  
  val version="0.92.4"
  
  def getPropertyAsInt(name:String,default:Int):Int = try {
    System.getProperties.getProperty(name).toInt
  } catch {
    case _ => default
  }
}
