//$Id$
package vcc.model.datastore

trait EntitySource {
  def getData():Iterator[Datum]
  def classId:String
  def id:String
}

class ListEntitySource(val classId:String,val id:String, lst:Seq[Datum]) extends EntitySource {
  def getData() = lst.elements
}

trait EntityDestination

object EntityLoader {
  
  def load(source:EntitySource):Entity = {
    if(!EntityFactory.isClassDefined(source.classId)) 
      throw new Exception("Cant find classId="+source.classId)
	val entity = EntityFactory.createInstance(source.classId,source.id)
    for(datum <- source.getData()) {
      entity.loadDatum(datum)
    }
    entity
  }
  
  def load():Entity = {
    /*
     * High level logic:
     * 
     * 1) Look on EntityIndex for this object will get A Datasouce
     * 2) Load From datasource
     * 
     */
    null
  }
  
  def store(ent:Entity) {
    /*
     

     */
  }
}
