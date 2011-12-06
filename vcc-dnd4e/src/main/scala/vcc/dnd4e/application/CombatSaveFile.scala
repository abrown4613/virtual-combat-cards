/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import vcc.dnd4e.tracker.common.CombatState
import java.io._
import scala.xml._

class CombatSaveFile {
  def load(inputStream: InputStream): CombatState = {
    val xml = XML.load(inputStream)
    val state = CombatState.empty
    val comment = (xml \ "comment").text
    state.copy(comment = if (comment!="") Some(comment) else None)
  }

  def save(outputStream: OutputStream, combatState: CombatState) {
    val writer: Writer = new PrintWriter(outputStream)
    val commentNode:NodeSeq = breakLine(combatState.comment.map(c => <comment>{c}</comment>).toList)
    val topLevel = Text("\n") :: commentNode :: Nil
    XML.write(writer, (<CombatSave>{topLevel}</CombatSave>), "UTF-8", true, null)
    writer.flush()
  }

  private def breakLine(nodeSeq: NodeSeq): NodeSeq = {
    if (!nodeSeq.isEmpty) Text("\t") ++ nodeSeq ++ Text("\n")
    else nodeSeq
  }
}