//$Id$
package vcc.model.datastore

abstract class Entity(val id:String) {
  
  val classId:String
  
  private var _containers = scala.collection.immutable.Map.empty[String,DataContainer] 
  
  val topLevel=new FieldSet(this,"base")

  def fieldSets:List[DataContainer]= _containers.map(x=>x._2).toList
  
  def addContainer(container:DataContainer) { _containers += (container.id -> container) }
  
  def extractData():List[Datum] = {
    _containers.flatMap(f=>f._2.exportData()).toList
  }
  
  def toXML:scala.xml.Node = <entity classId={classId} id={id}>{ _containers.map(c=>c._2.toXML).toSeq }</entity>
    
  def loadDatum(datum:Datum) { 
    if(_containers.contains(datum.prefix)) {
      _containers(datum.prefix).loadDatum(datum)
    } else throw new UnexistantField(classId,datum.prefix,datum.field)
  }
}
