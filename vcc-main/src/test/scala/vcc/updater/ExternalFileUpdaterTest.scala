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

import org.specs2.SpecificationWithJUnit
import org.exnebula.fileutil.FileWriter
import java.io.{InputStream, File}

class ExternalFileUpdaterTest extends SpecificationWithJUnit {
  def is = "ExternalFileUpdater".title ^
    "Update Info.plist if on mac" ! withMockWriter().mustUpdateInfoPlistOnMac ^
    "Update Info.plist if on mac with more subdir" ! withMockWriter().mustUpdateInfoPlistOnMacWithLongerPath ^
    "Dont attempt to update Info.plist if not mac" ! withMockWriter().mustSkipInfoPlistOnOthers ^
    "Dont attempt to update Info.plist if path has not .app" ! withMockWriter().mustSkipIfNotAppBundle ^
    end

  case class withMockWriter() {
    val updater = new MockFileWriter

    def mustUpdateInfoPlistOnMac = {
      val installDir = new File("/User/Some/Some app name.app/Contents/Resources/Java")
      ExternalFileUpdater.updateInfoPlist(installDir, updater)
      updater.whichFileWasStored must_== Some(new File("/User/Some/Some app name.app/Contents/Info.plist"))
    }

    def mustUpdateInfoPlistOnMacWithLongerPath = {
      val installDir = new File("/User/Some/Some app name.app/Contents/Resources/Java/repo/another")
      ExternalFileUpdater.updateInfoPlist(installDir, updater)
      updater.whichFileWasStored must_== Some(new File("/User/Some/Some app name.app/Contents/Info.plist"))
    }

    def mustSkipIfNotAppBundle = {
      val installDir = new File("/User/Some/other/Contents/Resources/Java/repo")
      ExternalFileUpdater.updateInfoPlist(installDir, updater)
      updater.whichFileWasStored must_== None
    }

    def mustSkipInfoPlistOnOthers = {
      val installDir = new File("/User/Some/other/path/lib")
      ExternalFileUpdater.updateInfoPlist(installDir, updater)
      updater.whichFileWasStored must_== None
    }
  }

  class MockFileWriter extends FileWriter {
    var whichFileWasStored:Option[File] = None

    override def storeFileFromStream(inputStream: InputStream, targetFile: File) {
      whichFileWasStored = Some(targetFile)
    }
  }
}