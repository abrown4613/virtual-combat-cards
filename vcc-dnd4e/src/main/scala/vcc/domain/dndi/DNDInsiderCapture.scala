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

import org.slf4j.LoggerFactory
import vcc.app.dndi.CaptureHoldingArea
import xml.{XML, Node}
import java.io.{FileOutputStream, File, InputStream}

object DNDInsiderCapture {
  private val logger = LoggerFactory.getLogger("domain")
  private val reSpaces = "[\\s\\n\\r\u00a0]+".r
  private val fixBadXML1 = " \\\\=\"\"".r
  private val handles = Set("monster","trap")

  private def load(xml: scala.xml.Node): DNDIObject = {
    if (xml == null) return null

    val id = getIdFromXML(xml)
    val clazz = getTypeFromXML(xml)
    if (id.isDefined && clazz.isDefined) {
      val reader: DNDIObjectReader[_] = clazz.get match {
        case "monster" => new MonsterReader(id.get)
        case "trap" => new TrapReader(id.get)
        case _ => null
      }

      if (reader != null) {
        try {
          val blocks = Parser.parseBlockElements(xml.child, true)
          val m = reader.read(blocks)
          logger.debug("Got entity: {}", m)
          m
        } catch {
          case e =>
            logger.debug("Failed to import class='{}' id='{}'.", Array(id.get, clazz.get), e)
            //THINK Should this be thrown
            throw e
        }
      } else {
        logger.debug("Reader for class='{}' not found.", Array(clazz.get), null)
        null
      }
    } else {
      //Id or class not defined, bad data from plugin or sender
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


  private def parseXML(is: InputStream, storeBadInput: Boolean): (Option[String], Option[Int], Node) = {
    // Capture XML
    var xmlRaw: String = null
    try {
      xmlRaw = pluginInputStreamAsFilteredString(is)
      logger.debug("Raw stream data: " + xmlRaw)
      val xml = XML.loadString(xmlRaw)
      (getTypeFromXML(xml), getIdFromXML(xml), xml)
    } catch {
      case s =>
        logger.warn("Failed to parse XML", s)
        logger.debug("XML Raw: {}", xmlRaw)
        if (storeBadInput && xmlRaw != null) {
          val file = File.createTempFile("capture", ".xml")
          try {
            val os = new FileOutputStream(file)
            os.write(xmlRaw.getBytes("UTF-8"))
            os.close()
            logger.warn("Writen to bad input to file {}", file.getAbsolutePath)
          } catch {
            case s =>
              logger.error("Failed to write bad input to {}", file.getAbsolutePath, s)
          }
        }
        (None, None, null)
    }
  }

  /**
   * Attempts to capture an entity using the default logic.
   * @param is The inputstream that contains the bytes of the supposed XML document. It will be filtered and converted
   * @param storeFailure Indicates if this method should store valid DNDI entries that where not loaded to full DNDIObject,
   * this normally happens when a type with no import logic is recieved (e.g. item, power, pc race or class).
   * @param storeBadInput Store data that failed to be parsed into XML, check log for where the file was saved.
   * @param sendToHoldingArea After a successfull input, if true will send the captured entity to the holding area. Set
   * to false for testing.
   * @return If None was returned, either the XML failed to parse, or it did not include class and ID for the entry.
   * If <code>Some(Left(pair))</code> was sent, you have a entry with ID and Class but no import logic.
   * If <code>Some(Right(obj))</code> was returned the entry was successfully imported.  
   */
  def captureEntry(is: InputStream, storeFailure: Boolean, storeBadInput: Boolean, sendToHoldingArea: Boolean): Option[Either[(String, Int), DNDIObject]] = {
    val (clazz, id, node) = parseXML(is, storeBadInput)
    if (node == null || !(clazz.isDefined && id.isDefined)) {
      // Failed to read XML or xml does not contain required fields
      None
    } else if(!handles.contains(clazz.get)) {
       //The object is of a class we dont capture yet.
      Some(Left(clazz.get,-1))
    } else {
      logger.debug("Parsed XML is: {}", node)
      val dndiObject = try {
        load(node)
      } catch {
        case e =>
          logger.error("Failed to import {} with id={}, reason", Array(clazz.get, id.get), e)
          null
      }
      if (dndiObject == null) {
        if (storeFailure) {
          // This is an option parameter to allow store objects that are not normally captured
          CaptureHoldingArea.storeIncompleteObject(clazz.get, id.get, node)
        }
        Some(Left(clazz.get, id.get))
      } else {
        if(sendToHoldingArea) CaptureHoldingArea.addCapturedEntry(dndiObject, node)
        Some(Right(dndiObject))
      }
    }
  }

  /**
   * This method will load an entry form stream, it provides less control over the process that captureEntry, and should
   * be used when you want the entity or nothing.
   * @param is The input stream
   * @return Will return an DNDIObject or null if something failed during loading.
   */
  def loadEntry(is: InputStream): DNDIObject = {
    try {
      DNDInsiderCapture.captureEntry(is, false, false, false) match {
        case None => null
        case Some(Left(p)) => null
        case Some(Right(obj)) => obj
      }
    } catch {
      case _ => null
    }
  }

  /**
   *  Get a Servlet request data InputStream and load it to a filtered String.
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

    var inTag = false
    val chars = new Array[Char](rawStr.length)
    rawStr.getChars(0, chars.length, chars, 0)
    for (i <- 0 to chars.length - 1) {
      if (inTag) {
        if (chars(i).isLetter || chars(i) == '/') chars(i) = chars(i).toUpper
        else inTag = false
      } else if (chars(i) == '<') inTag = true
    }
    val finalStr = new String(chars)
    finalStr
  }
}

