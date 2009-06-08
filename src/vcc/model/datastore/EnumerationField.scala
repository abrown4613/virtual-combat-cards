//$Id$
package vcc.model.datastore

class EnumerationConstantNotPresentException(enum:Enumeration,str:String) extends Exception {
  override def getMessage() = "Enumeration "+enum.getClass.getCanonicalName+ " does not contains value " + str
}

class EnumerationField[E<:Enumeration](fset:FieldContainer,id:String,enum:E) extends Field[E#Value](fset,id){

  private var _value:Option[E#Value]=None
  
  def value_= (v:E#Value) { _value=Some(v) }
  
  def clear { _value = None }
  
  def value:Option[E#Value] = _value
  
  def fromStorageString(str:String) {
    val v=enum.valueOf(str)
    if((str!=null && str!="") && ! v.isDefined) 
      throw new EnumerationConstantNotPresentException(enum,str) 
    _value=v
  }
  
  def toStorageString:String = if(_value.isDefined) _value.get.toString else null
  
  override def toString = "IntField("+prefix+":"+id+":= "+ value +")"
}
