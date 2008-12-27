//$Id$
package vcc.model

case class DefenseBlock(ac:Int,fortitude:Int,reflex:Int,will:Int) {
  def toXML = 
    <defense ac={ac.toString} fortitude={fortitude.toString} reflex={reflex.toString} will={will.toString} />
}

object DefenseBlock {
  import XMLLoaderUtilities._
  
  def fromXML(node: scala.xml.Node): DefenseBlock = 
    DefenseBlock(
      attribOrDefault[Int](node \ "@ac", 0,nodeSeq2Int),
      attribOrDefault[Int](node \ "@fortitude", 0,nodeSeq2Int),
      attribOrDefault[Int](node \ "@reflex", 0,nodeSeq2Int),
      attribOrDefault[Int](node \ "@will", 0,nodeSeq2Int))
}