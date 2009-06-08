//$Id$
package vcc.model.datastore

case class Datum(prefix:String,index:Int,field:String,value:String)

class UnexistantField(val classId:String, val prefix:String, val field:String) extends Exception {
  override def getMessage():String = "Unexistant field, class '"+ classId + "' does not contain field '"+prefix+":"+field+"'."
}

trait DataContainer {
  
  private[datastore] def exportData():List[Datum]
  
  def id:String
  
  def toXML:scala.xml.Node
  
  def loadDatum(datum:Datum)
}