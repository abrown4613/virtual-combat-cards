/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import scala.xml.{Node, NodeSeq}
import org.slf4j.LoggerFactory
import vcc.domain.dndi.Parser.BlockElement


trait BlockReader {
  def processBlock(block: BlockElement): Boolean

  def getObject: DNDIObject
}

object DNDInsiderCapture {
  val logger = LoggerFactory.getLogger("domain")

  def load(xml: scala.xml.Node): DNDIObject = {
    if (xml == null) return null

    val id = getIdFromXML(xml)
    if (id.isDefined) {
      val blocks = Parser.parseBlockElements(xml.child, true)

      val reader: BlockReader = getTypeFromXML(xml) match {
        case Some("monster") => new MonsterBuilder(new Monster(id.get))
        case _ => null
      }

      if (reader != null) {
        for (blk <- blocks) {
          try {
            reader.processBlock(blk)
          } catch {
            case e =>
              logger.debug("Failed to process block: " + blk)
              throw e
          }
        }
        reader.getObject
      } else {
        //TODO Alternative strategy transition till we have new block
        getTypeFromXML(xml) match {
          case Some("trap") =>
            val tr = new TrapReader(id.get)
            tr.process(blocks)
          case _ => null
        }
      }
    } else {
      //Id is not defined, bad data from plugin or sender
      return null
    }
  }

  def getTypeFromXML(xml: Node): Option[String] = {
    if ((xml \ "H1").isEmpty) None
    else {
      val hclass = (xml \ "H1")(0) \ "@class"
      if (hclass.isEmpty) None
      else Some(hclass.text)
    }
  }

  def getIdFromXML(xml: Node): Option[Int] = {
    if (xml.label == "DIV") {
      if ((xml \ "@id").isEmpty) None
      else try {
        Some((xml \ "@id")(0).toString.toInt)
      } catch {
        case _ => None
      }
    } else None
  }

  private final val reSpaces = "[\\s\\n\\r\u00a0]+".r
  private final val fixBadXML1 = " \\\\=\"\"".r

  /**
   * Get a Servlet request data InputStream and load it to a filtered String.
   * It removes bad backslash and reduces several &nnbsp; (Unicode \u00a0), \n, \r to a single space.
   * @para in InputStream, most likely from Servlet request.getInputStream
   * @return The filter UTF-8 block
   */
  def pluginInputStreamAsFilteredString(in: java.io.InputStream): String = {
    val bout = new java.io.ByteArrayOutputStream();
    val buffer = new Array[Byte](1024);
    var len = 0

    while ({len = in.read(buffer); len} > 0) {
      bout.write(buffer, 0, len);
    }
    val data = bout.toByteArray()
    var rawStr = new String(data, "UTF-8")
    rawStr = reSpaces.replaceAllIn(rawStr, " ")
    rawStr = fixBadXML1.replaceAllIn(rawStr, "")

    var intag = false
    var chars = new Array[Char](rawStr.length)
    rawStr.getChars(0, chars.length, chars, 0)
    for (i <- 0 to chars.length - 1) {
      if (intag) {
        if (chars(i).isLetter || chars(i) == '/') chars(i) = chars(i).toUpper
        else intag = false
      } else if (chars(i) == '<') intag = true
    }
    val finalStr = new String(chars)
    finalStr
  }
}

