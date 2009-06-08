//$Id$
package vcc.model.datastore

abstract class Field[T](val fset:FieldContainer, val id:String) {
  
  def value:Option[T]
  
  def value_=(v:T)
  
  def clear()
  
  fset.addField(this)
  
  def fromStorageString(str:String)
  
  def toStorageString:String
  
  def prefix:String = fset.storageId + ":" + fset.storageIndex

  def extractData():List[Datum] = List(Datum(fset.storageId,fset.storageIndex,id,toStorageString))

  def toXML:scala.xml.Node = {
    val datum=toStorageString
    if(datum!=null) <datum id={id}>{datum}</datum>
    else null
  }
}
