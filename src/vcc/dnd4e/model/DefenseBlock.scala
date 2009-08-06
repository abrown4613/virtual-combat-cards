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
package vcc.dnd4e.model

/**
 * A set of defense stats.
 * @param ac Armor class
 * @param fortitude Fortitude defense
 * @param reflex Reflex defense
 * @param will Will defense
 */
case class DefenseBlock(ac:Int,fortitude:Int,reflex:Int,will:Int) {
  def toXML = 
    <defense ac={ac.toString} fortitude={fortitude.toString} reflex={reflex.toString} will={will.toString} />
}

object DefenseBlock {
  import vcc.util.XMLHelper._
  
  def fromXML(node: scala.xml.Node): DefenseBlock = 
    DefenseBlock(
      nodeSeq2Int(node \ "@ac", 0),
      nodeSeq2Int(node \ "@fortitude", 0),
      nodeSeq2Int(node \ "@reflex", 0),
      nodeSeq2Int(node \ "@will", 0))
}