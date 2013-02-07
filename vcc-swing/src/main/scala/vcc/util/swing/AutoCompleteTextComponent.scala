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

import scala.swing._
import javax.swing.{AbstractAction, KeyStroke}
import java.awt.event.ActionEvent
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.BadLocationException

/**
 * This mixin trait can be added to any TextComponent to enable auto completion on it.
 */
trait AutoCompleteTextComponent extends DocumentListener {
  self: TextComponent =>

  private final val CommitAction = "AutoCompleteTextComponent.CommitAction"

  /**
   * Used to indicate that we have a completion showing
   */
  private var inCompletionMode = false
  /**
   * Used to indicate that the DocumentListener should not try to handle the event because we are inserting a completion
   */
  private var insertingCompletion = false

  /**
   * Dictionary of completion
   */
  private var dictionary: AutoCompleteDictionary = null

  /**
   * Turn on auto complete based on the dictionary provided
   * @param dictionary Dictionary of words to be used for auto completion
   */
  def enableAutoComplete(dictionary: AutoCompleteDictionary) {
    val im = self.peer.getInputMap
    val am = self.peer.getActionMap
    im.put(KeyStroke.getKeyStroke("ENTER"), CommitAction)
    am.put(CommitAction, new CommitAction())

    self.peer.getDocument.addDocumentListener(this)
    self.dictionary = dictionary
  }

  /**
   * Class used to handle enter while during auto completion.
   */
  private class CommitAction extends AbstractAction {
    def actionPerformed(e: ActionEvent) {
      if (inCompletionMode) {
        val pos = self.peer.getSelectionEnd
        self.peer.getDocument.insertString(pos, " ", null)
        self.peer.setCaretPosition(pos + 1)
        inCompletionMode = false
      } else {
        self.peer.replaceSelection("\n")
      }
    }
  }

  def changedUpdate(e: DocumentEvent) {}

  def removeUpdate(e: DocumentEvent) {}

  def insertUpdate(ev: DocumentEvent) {
    /**
     *  Backtrack from a given position to find the first the word prefix. This is done by looking for the first non
     * letter character from the current position to the start of the string.
     * @param text String to search
     * @param pos Position
     */

    def getWordPrefix(text: String, pos: Int): String = {
      var p = pos
      while (p >= 0 && Character.isLetter(text.charAt(p))) {
        p -= 1
      }
      text.substring(p + 1, pos + 1)
    }
    //Begin function
    if (ev.getLength != 1 || insertingCompletion) return

    val pos = ev.getOffset

    val content = try {
      self.peer.getText(0, pos + 1)
    } catch {
      case e: BadLocationException => return
    }

    val prefix = getWordPrefix(content, pos)

    if (prefix.length < 2) return // Too short
    val suggestion = dictionary.findSuggestion(prefix)

    if (suggestion.isDefined) {
      val completion = suggestion.get.substring(prefix.length)
      SwingHelper.invokeLater{
        insertingCompletion = true
        self.peer.getDocument.insertString(pos + 1, completion, null)
        self.peer.setCaretPosition(pos + 1 + completion.length)
        self.peer.moveCaretPosition(pos + 1)
        inCompletionMode = true
        insertingCompletion = false
      }
    }
  }
}
