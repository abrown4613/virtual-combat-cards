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
package vcc.dnd4e.view.dialog

import javax.swing.{JComponent, JFileChooser, JOptionPane}
import javax.swing.filechooser.{FileFilter, FileNameExtensionFilter}
import java.io.File

/**
 * Series of utility methods for creating File dialogs.
 */
object FileChooserHelper {

  private var lastDirectory = new File(System.getProperty("user.dir"))

  def setLastDirectory(newDirectory: File) {
    synchronized {
      lastDirectory = newDirectory
    }
  }

  /**
   * Filter files that are party. 
   */
  val partyFilter = new FileNameExtensionFilter("XML Files", "peml", "xml")

  /**
   * Filter for DND4E file (.dnd4e)
   */
  val characterBuilderFilter = new FileNameExtensionFilter("Character Builder Files", "dnd4e")

  protected def normalizeFileName(file: File, filter: FileFilter): File = {
    filter match {
      case `partyFilter` =>
        val filename = file.getAbsolutePath
        if (!filename.endsWith(".xml") && !filename.endsWith(".peml"))
          new File(filename + ".peml")
        else file
    }
  }

  protected def confirmOverwrite(file: File): Option[File] = {
    if (file.exists) {
      val res = JOptionPane.showConfirmDialog(
        null, "Are you sure you want to overwrite " + file.getAbsolutePath + "?",
        "Overwrite File?", JOptionPane.YES_NO_OPTION)
      if (res == JOptionPane.YES_OPTION) Some(file)
      else None
    } else
      Some(file)
  }

  private def getWorkDirectory: File = lastDirectory

  /**
   * Open a Save Dialog, get file, and normalize name to add extension.
   * @param over Component to place over (for MainFrame, use frame.peer.getRootPane)
   * @param filter Optional file filter.
   * @return Return None on a cancel, Some(file) otherwise
   */
  def chooseSaveFile(over: JComponent, filter: FileFilter): Option[File] = {
    val fileDialog = new JFileChooser(getWorkDirectory)
    if (filter != null) fileDialog.setFileFilter(filter)

    val result = fileDialog.showSaveDialog(over)
    if (result == JFileChooser.APPROVE_OPTION) {
      val file = normalizeFileName(fileDialog.getSelectedFile, filter)
      setLastDirectory(fileDialog.getSelectedFile.getParentFile)
      confirmOverwrite(file)
    } else
      None
  }

  /**
   * Open a file Open Dialog, get selection.
   * @param over Component to place over (for MainFrame, use frame.peer.getRootPane)
   * @param filter Optional file filter.
   * @return Return None on a cancel, Some(file) otherwise
   */
  def chooseOpenFile(over: JComponent, filter: FileFilter): Option[File] = {
    val fileDialog = new JFileChooser(getWorkDirectory)
    if (filter != null) fileDialog.setFileFilter(filter)

    val result = fileDialog.showOpenDialog(over)
    if (result == JFileChooser.APPROVE_OPTION) {
      setLastDirectory(fileDialog.getSelectedFile.getParentFile)
      Some(fileDialog.getSelectedFile)
    } else
      None
  }
}
