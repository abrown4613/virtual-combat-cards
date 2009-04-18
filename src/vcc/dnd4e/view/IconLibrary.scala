//$Id$
package vcc.dnd4e.view

import javax.swing.ImageIcon

/**
 * This is a collection of images loaded for the view compoents.
 */
object IconLibrary {
  protected def loadIcon(resource:String):ImageIcon = {
    val url=this.getClass.getResource(resource)
    if(url!=null) {
      val icon=new ImageIcon(url)
      icon
    } else {
      println("Failed to load resource: "+resource)
      exit(1)
    }
  }
 
  val MetalD20 = loadIcon("/vcc/dnd4e/images/d20metal.png")
}
