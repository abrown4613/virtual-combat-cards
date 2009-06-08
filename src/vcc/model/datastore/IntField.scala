//$Id$
package vcc.model.datastore

class IntField(override val fset:FieldContainer, override val id:String) extends Field[Int](fset,id) {

  private var _value:Option[Int]=None
  
  def value_= (v:Int) { _value=Some(v) }
  
  def clear { _value = None }
  
  def value:Option[Int] = _value
  
  def fromStorageString(str:String) {
    if(str!=null && str!="") {
      try {
        _value=Some(str.toInt)
      } catch {
        case s => throw s
      }
    } else _value=None
  }
  
  def toStorageString:String = if(_value.isDefined) _value.get.toString else null
  
  override def toString = "IntField("+prefix+":"+id+":= "+ value +")"
}
