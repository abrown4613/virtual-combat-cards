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

package vcc.util

import java.io.File

/**
 * Provides an Itereator fo a Directory tree. Used to assemble list, or map a function to
 * all the files.
 * @param dir The file or directory to start from
 * @param dirFirst Place the current directory in start of the list
 */
class DirectoryIterator(dir:File, dirFirst:Boolean) extends Iterator[File] {
  class SubIterator(subdir:File, parent:SubIterator) {
    val iter = makeFileList()
    
    def makeFileList():Iterator[File] = {
      val list:List[File] = 
        if(subdir.exists && subdir.isDirectory) 
          subdir.list().map(x=>new File(subdir,x)).toList
        else Nil
      (if(dirFirst) subdir :: list
      else list ::: List(subdir)).iterator
    }
    
    def hasNext:Boolean = iter.hasNext || (parent != null && parent.hasNext)
    def next:(SubIterator,File) = {
      if(iter.hasNext) {
    	val file=iter.next
        if(file.exists && file.isDirectory && file != subdir ) new SubIterator(file,this).next
        else (this,file)
      } else {
        parent.next
      }
    } 
  }
  
  private var si = new SubIterator(dir,null) 
  
  def hasNext = si.hasNext
  
  def next:File = {
    val (nsi,file)=si.next
    si=nsi
    file
  }
}

