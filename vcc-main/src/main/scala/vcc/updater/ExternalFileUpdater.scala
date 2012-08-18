/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.updater

import org.exnebula.fileutil.FileWriter
import java.io.File
import annotation.tailrec

object ExternalFileUpdater {
  def updateInfoPlist(installDir: File, writer: FileWriter) {
    @tailrec
    def backTrackToContent(file: File):File = {
      if (file.getName == "Contents") file
      else backTrackToContent(file.getParentFile)
    }
    val pathParts = filePathToList(installDir)
    if (installDir.getAbsolutePath.contains(".app") && containsSubList(pathParts, List("Contents","Resources", "Java"))) {
      writer.storeFileFromStream(
        this.getClass.getClassLoader.getResource("external/Info.plist").openStream(),
        new File(backTrackToContent(installDir), "Info.plist")
      )
    }
  }

  private def headsMatchForLengthOfSecond[T](toMatch: List[T], second:List[T]):Boolean = {
    second match {
      case _ if (toMatch.isEmpty) => second.isEmpty
      case head :: tail =>
        if (toMatch.head == head) headsMatchForLengthOfSecond(toMatch.tail, tail)
        else false
      case Nil => true
    }
  }

  private def containsSubList[T](inspected: List[T], searchList:List[T]): Boolean = {
    inspected match {
      case head :: tail =>
         if (head == searchList.head) headsMatchForLengthOfSecond(tail, searchList.tail)
         else containsSubList(tail, searchList)
      case Nil => false
    }
  }

  private def filePathToList(path: File):List[String] = {
    var parent:File = path
    var result:List[String] = Nil
    while(parent != null) {
      result = parent.getName :: result
      parent = parent.getParentFile
    }
    result.tail
  }
}