//$Id$
package vcc.model.datastore

class StringField(override val fset:FieldContainer, override val id:String) extends Field[String](fset,id)  {

  var _value:String=null

  def value_=(value:String) { _value=value }
  
  def value: Option[String] = if(_value!=null) Some(_value) else None
  
  def clear() { _value = null }
  
  def fromStorageString(str:String) {
    _value=str
  }
  
  def toStorageString:String = _value
  
  override def toString:String = "StringField("+prefix +":"+id+ ":="+ value +")"
}
