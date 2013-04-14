/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dndi.reader

import java.io.InputStream

object CaptureInputFilter {

  private val reSpaces = "[\\s\\n\\r\u00a0]+".r
  private val fixBadXML1 = """ \\=""""".r

  def filterInput(in:InputStream):String = {
    var rawStr = inputToString(in)
    rawStr = reSpaces.replaceAllIn(rawStr, " ")
    rawStr = fixBadXML1.replaceAllIn(rawStr, "")
    xmlTagToUpperCase(rawStr)
  }


  private def xmlTagToUpperCase(rawStr: String): String = {
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

  private def inputToString(in: InputStream): String = {
    val bout = new java.io.ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    var len = 0

    while ( {
      len = in.read(buffer)
      len
    } > 0) {
      bout.write(buffer, 0, len)
    }
    new String(bout.toByteArray, "UTF-8")
  }
}