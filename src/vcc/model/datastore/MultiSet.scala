//$Id$
package vcc.model.datastore

class MultiSetFieldContainer(val parent:MultiSet[_]) extends FieldContainer {
  
  override def id:String = storageIndex().toString
  
  private[datastore] def storageId():String = parent.id
  
  private[datastore] def storageIndex():Int = parent.indexOf(this)

  private[datastore] def classId():String = parent.owner.classId

}

class MultiSet[T<:MultiSetFieldContainer](val owner:Entity,val id:String,constructor:()=>T) extends DataContainer {
  var _elem:List[T]=Nil
  
  owner.addContainer(this)
  
  def apply(idx:Int):T =  _elem(idx)
  
  def addInstance():Int = {
    _elem = _elem ::: List(constructor())
    _elem.length-1
  }
  
  def indexOf(elem:MultiSetFieldContainer) = _elem.indexOf(elem)
  
  override def exportData():List[Datum] = {
    _elem.flatMap(e=>e.exportData())
  }
  
  override def toString():String = "MultiSet("+id+")"
  
  def toXML:scala.xml.Node= <mset id={id}>{_elem.map(e=>e.toXML).toSeq}</mset>

  def loadDatum(datum:Datum) {
    //Fill up table
    while(_elem.length-1 < datum.index) addInstance()
    _elem(datum.index).loadDatum(datum)
  }
  
}
