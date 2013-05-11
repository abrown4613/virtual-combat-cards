/*
 * Copyright (C) 2018-2013 - Thomas Santana <tms@exnebula.org>
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

import java.io.File
import org.specs2.SpecificationWithJUnit
import scala.xml.XML
import vcc.dndi.reader.Parser._

class MonsterMassLoadTest extends SpecificationWithJUnit {
  private val pathToTest = System.getProperty("test.basedir")

  def is = if (pathToTest != null) "Import" ^ xx ^ end
  else "Skipped since test.basedir is not set" ! success

  def xx = {
    val dirIter = new DirectoryIterator(new File(pathToTest))
    (for (file <- dirIter if (file.isFile)) yield {
      "Loading " + file.getCanonicalPath ! load(file)
    }).toSeq
  }

  def load(file: File) = (loadFile(file) must not be null)

  def loadFile(file: File):AnyRef = try {
    val xml = XML.loadFile(file)
    val blocks = parseBlockElements(xml.child)
    if (blocks != null && !blocks.isEmpty) {
      new MonsterReader(0).process(blocks)
    } else {
      null
    }
  } catch {
    case e:Exception => null
  }

  class DirectoryIterator(dir: File) extends Iterator[File] {

    class SubIterator(subdir: File, parent: SubIterator) {
      val iter = makeFileList()

      def makeFileList(): Iterator[File] = {
        val list: List[File] =
          if (subdir.exists && subdir.isDirectory)
            subdir.list().map(x => new File(subdir, x)).toList
          else Nil
        (list ::: List(subdir)).iterator
      }

      def hasNext: Boolean = iter.hasNext || (parent != null && parent.hasNext)

      def nextRecursive: (SubIterator, File) = {
        if (iter.hasNext) {
          val file = iter.next()
          if (file.exists && file.isDirectory && file != subdir) new SubIterator(file, this).nextRecursive
          else (this, file)
        } else {
          parent.nextRecursive
        }
      }
    }

    private var si = new SubIterator(dir, null)

    def hasNext = si.hasNext

    def next(): File = {
      val (nsi, file) = si.nextRecursive
      si = nsi
      file
    }
  }

}