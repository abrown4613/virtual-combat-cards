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
package vcc.util.swing

import java.io.InputStream
import io.Source
import collection.mutable.ListBuffer

/**
 * Provides a wrapper and interface to register auto-completion for the AutoCompleteTextComponent
 */
class AutoCompleteDictionary(private val words: List[String]) {

  /**
   * Find a suggestion from a given prefix.
   * @param prefix Prefix to search for.
   */
  def findSuggestion(prefix: String): Option[String] = {
    words.find(word => word.startsWith(prefix.toLowerCase))
  }
}

/**
 * Service object used to load dictionary from files.
 */
object AutoCompleteDictionary {
  def fromStream(is: InputStream): AutoCompleteDictionary = fromStream(is, null)

  /**
   * Read stream as a text file and load all the lines in sequence. Will check for prefix (words) that are being
   * redefined (since they will not be matched). Will trim lines and ignore blank lines (or lines with only spaces).
   * Will ignore lines that begin with # (pound/sharp sign)
   * This operation os O(n*n)
   * @param is Input stream to be read, assumes a text stream with one definition per line
   * @param warningHandler Optional function to be called when a some term will not be handled as expected, for exampled
   * there is a repeated prefix. First parameter is the term, second is the error message.
   * @return A dictionary with all the valid lines in the order they where placed in the file.
   */
  def fromStream(is: InputStream, warningHandler: (String, String) => Unit): AutoCompleteDictionary = {
    val words = new ListBuffer[String]()
    for (line <- Source.fromInputStream(is).getLines()) {
      val trimmed = line.trim
      if (trimmed.length > 0 && !trimmed.startsWith("#")) {
        val prefix = trimmed.split(" ")(0)
        if (words.exists(w => w.startsWith(prefix)) && warningHandler != null)
          warningHandler(trimmed, "Prefix '" + prefix + "' already defined")

        words += trimmed
      }
    }
    new AutoCompleteDictionary(words.toList)
  }
}