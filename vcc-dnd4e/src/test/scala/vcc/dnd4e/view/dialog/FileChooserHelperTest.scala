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

import org.uispec4j.{Window, Trigger, UISpecTestCase}
import org.uispec4j.interception.{FileChooserHandler, WindowHandler, WindowInterceptor}
import java.io.File
import junit.framework.Assert
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.UIManager

class FileChooserHelperTest extends UISpecTestCase {

  private val saveTitle = getLabel("FileChooser.saveDialogTitleText")
  private val yesButtonLabel = getLabel("OptionPane.yesButtonText")
  private val noButtonLabel = getLabel("OptionPane.noButtonText")

  private var result: Option[File] = None

  def testShowSaveDialogAndSetPartialFileName() {
    runSaveOperation("file")
    validateReturnedAbsoluteFilename(new File("file.peml"))
  }

  def testShowSaveDialogAndSetCompleteFilename() {
    runSaveOperation("file.peml")
    validateReturnedAbsoluteFilename(new File("file.peml"))
  }

  def testShowSaveDialogAndSetCompleteFilename2() {
    runSaveOperation("file.xml")
    validateReturnedAbsoluteFilename(new File("file.xml"))
  }

  def testShowSaveDialog_uponSelectingExistentFilePromptForOverwrite() {
    val preExistentFile = new File("file2.peml")
    runSaveOperationWithOverwrite(preExistentFile, yesButtonLabel)
    validateReturnedAbsoluteFilename(preExistentFile)
  }

  def testShowSaveDialog_uponSelectingExistentFilePromptForOverwriteButNegate() {
    val preExistentFile = new File("file2.peml")
    preExistentFile.createNewFile()
    runSaveOperationWithOverwrite(preExistentFile, noButtonLabel)
    preExistentFile.delete()
    Assert.assertEquals(None, result)
  }

  private def validateReturnedAbsoluteFilename(file: File) {
    Assert.assertEquals(file.getAbsoluteFile, result.get.getAbsoluteFile)
  }

  private def runSaveOperation(selectedFileName: String) {
    showSaveDialog(result, FileChooserHelper.partyFilter).process(checkTitleAndSelectAnswer(saveTitle, selectedFileName)).run()
  }

  private def getLabel(resource: String)= UIManager.get(resource).asInstanceOf[String]

  def runSaveOperationWithOverwrite(preExistentFile: File, overwriteAnswer: String) {
    preExistentFile.createNewFile()
    showSaveDialog(result, FileChooserHelper.partyFilter).
      process(checkTitleAndSelectAnswer(saveTitle, preExistentFile.getName)).
      process(expectOverwriteWarningAndOverride(overwriteAnswer)).
      run()
    preExistentFile.delete()
  }

  private def showSaveDialog(_result: Option[File], filter: FileNameExtensionFilter): WindowInterceptor = {
    WindowInterceptor.init(new Trigger {
      def run() {
        result = FileChooserHelper.chooseSaveFile(null, filter)
      }
    })
  }

  private def checkTitleAndSelectAnswer(title: String, fileNameText: String): WindowHandler = {
    FileChooserHandler.init()
      .titleEquals(title)
      .assertAcceptsFilesOnly()
      .select(fileNameText)
  }

  private def expectOverwriteWarningAndOverride(answer: String) = {
    new WindowHandler() {
      def process(window: Window): Trigger = {
        assertTrue(window.titleContains("Overwrite File?"))
        window.getButton(answer).triggerClick()
      }
    }
  }
}