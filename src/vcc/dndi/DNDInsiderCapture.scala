//$Id$
package vcc.dndi

object DNDInsiderCapture {
  final val colTrim=new scala.util.matching.Regex("^:?\\s*(\\S.*\\S)\\s*$")
  final val flexiIntReg=new scala.util.matching.Regex("^\\s*([\\+\\-])?\\s*(\\d+)\\s*[\\,\\;]?\\s*$")
  
  /**
   * Extract int form fields like:
   * + 20
   * - 5
   * - 11 ,
   */
  def flexiToInt(s:String):Option[Int] = {
    s match {
      case this.flexiIntReg(signal,number)=> Some(if(signal=="-") - number.toInt else number.toInt)
      case _ => None
    }
  } 
  
  def captureTrim(s:String):String = {
    s match {
      case this.colTrim(r) => r
      case s => s
    }
  }

  def load(xml:scala.xml.Node):DNDIObject = {
    if(xml==null) return null
    
    //val ns= xml.child.filter(node=>(node.label=="P" || node.label=="H1"))
    
    (xml \\ "H1" \"@class").toString match {
      case "monster" => new Monster(xml)
      case _ => null
    }
  }
}

trait DNDIObject {
  def apply(attribute:String):Option[String]
}
