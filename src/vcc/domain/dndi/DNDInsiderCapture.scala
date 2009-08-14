/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package vcc.domain.dndi

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
    
    val id = try {(xml \ "@id").toString.toInt } catch { case _ => 0}
    (xml \\ "H1" \"@class").toString match {
      case "monster" => new Monster(xml,id)
      case _ => null
    }
  }
}

trait DNDIObject {
  def apply(attribute:String):Option[String]
}
