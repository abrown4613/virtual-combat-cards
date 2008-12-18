package vcc.util.swing

abstract class TableModelRowProjection[T] {
  val columns:List[(String,java.lang.Class[_])]
  def size = columns.size
  val setter:PartialFunction[(Int,T,Any),Unit]
  def getColumnClass(col:Int):java.lang.Class[_]=columns(col)._2
  def getColumnName(col:Int):String=columns(col)._1
  def isEditable(col:Int,obj:T)=if(setter!=null)setter.isDefinedAt(col,null.asInstanceOf[T],null) else false
  def apply(col:Int,obj:T):java.lang.Object
  def set(col:Int,obj:T,value:Any):Unit=if(setter!=null && setter.isDefinedAt(col,obj,value)) setter(col,obj,value)
}


class ProjectionTableModel[T](val proj:TableModelRowProjection[T]) extends javax.swing.table.AbstractTableModel {
  var elem:Seq[T]=Nil
  def getValueAt(row:Int,col:Int):java.lang.Object= if(elem.size>=row )proj(col,elem(row)) else null
  def getColumnCount():Int = proj.size
  def getRowCount():Int=elem.size
  override def getColumnName(col:Int)=proj.getColumnName(col)
  override def getColumnClass(col:Int)=proj.getColumnClass(col)
  override def isCellEditable(row:Int,col:Int)=(elem.size>=row) && proj.isEditable(col,elem(row))
  override def setValueAt(value:java.lang.Object,row:Int,col:Int)=proj.set(col,elem(row),value)
  
  def content_=(content:Seq[T])={elem=content; this.fireTableDataChanged}
  def content=elem
}
