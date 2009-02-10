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
    val fileDiag= new JFileChooser(new java.io.File(System.getProperty("user.dir")))
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
    val fileDiag= new JFileChooser(new java.io.File(System.getProperty("user.dir")))
    if(filter!=null) fileDiag.setFileFilter(filter)
    
    val result=fileDiag.showOpenDialog(over)
    if(result==JFileChooser.APPROVE_OPTION) 
      Some(fileDiag.getSelectedFile)
    else 
      None
  }

}
