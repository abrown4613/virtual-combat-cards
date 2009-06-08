//$Id$
package vcc.util

import scala.xml.NodeSeq

object XMLHelper {
  
  def nodeSeq2String(ns:NodeSeq):String = { 
    if(ns != null && ns.length==1) ns.text
    else throw new Exception("Invalid NodeSeq, should contains a single node")
  } 

  def nodeSeq2String(ns:NodeSeq,default:String):String = { 
    if(ns != null && ns.length==1) ns.text
    else default
  } 
  
  def nodeSeq2Int(ns:NodeSeq) = nodeSeq2String(ns).toInt

  def nodeSeq2Int(ns:NodeSeq,default:Int) = try{nodeSeq2String(ns).toInt}catch{case _ => default}

}
