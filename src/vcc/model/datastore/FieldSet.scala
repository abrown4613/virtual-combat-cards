//$Id$
package vcc.model.datastore

trait FieldContainer extends DataContainer {

  private var _fields=scala.collection.immutable.Map.empty[String,Field[_]]
  
  def fields:List[Field[_]] = _fields.map(f=>f._2).toList
  
  def fieldNames:List[String] = _fields.map(f=>f._1).toList
  
  def addField(field:Field[_]) { _fields= _fields + (field.id -> field)}
  
  private[datastore] def classId():String
  
  private[datastore] def storageId():String
  
  private[datastore] def storageIndex():Int

  private[datastore] def exportData():List[Datum] = _fields.flatMap({f=>f._2.extractData()}).toList
  
  def toXML:scala.xml.Node = <set id={id}>{_fields.map(f=>f._2.toXML).toSeq}</set>

  def loadDatum(datum:Datum) {
	if(_fields.contains(datum.field)) {
	  _fields(datum.field).fromStorageString(datum.value)
	} else {
	  throw new UnexistantField(classId,datum.prefix,datum.field)
	}
  }
  
}

class FieldSet(val owner:Entity,val id :String) extends FieldContainer {

  owner.addContainer(this)
  
  private[datastore] val classId = owner.classId
  
  override def toString:String = "FieldSet("+id+")"
  
  private[datastore] def storageId():String = id
  
  private[datastore] def storageIndex():Int = 0

}