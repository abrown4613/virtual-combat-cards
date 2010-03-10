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
package vcc.dnd4e.view.dialog

import javax.swing.{JComponent,JFileChooser,JOptionPane}
import javax.swing.filechooser.{FileFilter,FileNameExtensionFilter}
import java.io.File

/**
 * Series of utility methods for creating dailogs
 * 
 */
object FileChooserHelper {
  
  /**
   * Filter files that are party. 
   */
  val partyFilter=new javax.swing.filechooser.FileNameExtensionFilter("XML Files","peml","xml")
  
  protected def normalizeFileName(file:File,filter:FileFilter):File = {
    filter match {
      case `partyFilter` => 
        var filename=file.getAbsolutePath
        if(!filename.endsWith(".xml") && !filename.endsWith(".peml")) 
          new File(filename+".peml")
        else file
    }
  }
  
  protected def confirmOverwrite(file:File):Option[File] = {
    if(file.exists) {
      var res=JOptionPane.showConfirmDialog(
        null,"Are you sure you want to overwrite "+file.getAbsolutePath+"?",
        "Overwrite File?", JOptionPane.YES_NO_OPTION)
      if(res== JOptionPane.YES_OPTION) Some(file)
      else None
    } else 
      Some(file)
  }
  
  /**
   * Open a Save Dialog, get file, and normlize name to add extension.
   * Return None on a cancel.
   */
  def chooseSaveFile(over:JComponent,filter:FileFilter):Option[java.io.File] = {
    val fileDiag= new JFileChooser(Configuration.baseDirectory.value)
    if(filter!=null) fileDiag.setFileFilter(filter)
    
    val result=fileDiag.showSaveDialog(over)
    if(result==JFileChooser.APPROVE_OPTION) {
      var file=normalizeFileName(fileDiag.getSelectedFile,filter)
      confirmOverwrite(file)
    } else 
      None
  }

  /**
   * Open a Open Dialog, get selection. 
   * Return None on a cancel.
   */
  def chooseOpenFile(over:JComponent,filter:FileFilter):Option[java.io.File] = {
    val fileDiag= new JFileChooser(Configuration.baseDirectory.value)
    if(filter!=null) fileDiag.setFileFilter(filter)
    
    val result=fileDiag.showOpenDialog(over)
    if(result==JFileChooser.APPROVE_OPTION) 
      Some(fileDiag.getSelectedFile)
    else 
      None
  }

}
