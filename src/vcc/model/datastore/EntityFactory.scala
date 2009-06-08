//$Id$
package vcc.model.datastore

/**
 * 
 */
object EntityFactory {
  
  type Constructor = String => Entity
  
  private var _registry=scala.collection.immutable.Map.empty[String,Constructor]
  
  /**
   * Register a class and constructor for a given class. Will replace prior definition
   * @param classId The name of the class to be registers
   * @param constructor A function that returns of Entity of 
   */
  def registerEntityClass(classId:String,constructor:Constructor) {
    _registry += classId->constructor
  }
  
  /**
   * Check if the class is registered on the Factory
   * @parma classId Class name to get
   * @return true if the class is register
   */
  def isClassDefined(classId:String):Boolean = _registry.contains(classId)
  
  /**
   * Create a new instance of the class if it's registered
   */
  def createInstance(classId:String, id:String):Entity = {
    if(isClassDefined(classId)) _registry(classId)(id)
    else null
  }

}
