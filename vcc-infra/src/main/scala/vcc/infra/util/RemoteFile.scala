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
//$Id$
package vcc.infra.util

import java.net.URL
import java.io._
import java.nio.channels.{ReadableByteChannel, Channels}

object RemoteFile {

  protected def loadToMemoryStream(file: File): InputStream = {
    var is: InputStream = null
    try {
      val buffer = new Array[Byte](file.length().toInt)
      is = new FileInputStream(file)
      is.read(buffer)
      is.close()
      new ByteArrayInputStream(buffer)
    }
    catch {
      case s =>
        null
    }
    finally {
      if (is != null) is.close()
    }
  }

  class LocalFile(file: File) {
    /**
     * Last modification of the file if it exists
     * @return Time in milliseconds of from start of unix epoch
     */
    def getModificationTime: Long = {
      if (file.exists() && file.isFile && file.canRead)
        file.lastModified()
      else
        0L
    }

    /**
     * Return a InputStream to the memory representation of the file.
     */
    def loadToMemoryStream: InputStream = {
      RemoteFile.loadToMemoryStream(file)
    }

    /**
     * Save a stream to replace current version of the file.
     */
    def saveFromStream(is: InputStream) {
      var outChannel: FileOutputStream = null;
      try {
        outChannel = new FileOutputStream(file);
        val inChannel = Channels.newChannel(is)
        outChannel.getChannel.transferFrom(inChannel, 0, 1 << 24)
      }
      catch {
        case s =>
      }
      finally {
        outChannel.close()
      }
    }

  }

  class RemoteSource(remoteURL: URL) {

    /**
     * Return stream to the memory loaded content of the file. During load it will generate a temporary file, after a
     * successful load the content of the file is loaded to memory.
     */
    def fetchContent: InputStream = {
      var file: File = null
      var rbc: ReadableByteChannel = null
      var fos: FileOutputStream = null
      try {
        file = File.createTempFile("wget", "dat")
        rbc = Channels.newChannel(remoteURL.openStream())
        fos = new FileOutputStream(file)
        fos.getChannel.transferFrom(rbc, 0, 1 << 24)
        loadToMemoryStream(file)
      }
      catch {
        case e =>
          null
      }
      finally {
        if (file != null) file.deleteOnExit()
        if (fos != null) fos.close()
        if (rbc != null) rbc.close()
      }
    }
  }

}

/**
 * Local copy of a file that exists somewhere in the web, this is used to cache a local copy of files. Notice that
 * files will be loaded to memory when fetched so avoid large file.
 */
class RemoteFile private[util](localFile: RemoteFile.LocalFile, remoteSource: RemoteFile.RemoteSource) {

  /**
   * Build a RemoteFile based on the File and remote URL
   * @param localFile File representation of the local cached version of the file
   * @param remoteURL Where to fetch updates from
   */
  def this(localFile: File, remoteURL: URL) = this (new RemoteFile.LocalFile(localFile), new RemoteFile.RemoteSource(remoteURL))

  /**
   * Return the current value of the local copy, if it exists:
   * @return A stream to the memory where the file content was loaded, null if the loaded failed.
   */
  def getLocalCopy: InputStream = localFile.loadToMemoryStream

  /**
   * Load the file to memory and return a stream to its content, if the file is older than age supplied will attempt to
   * fetch a copy of the remote file. On a failure to get the remote file, the local version will be served. If remote
   * is fetched the new content will be identical.
   * @param age Maximum age of the file in milliseconds.
   * @return Stream to memory loaded file or null if no copy was found.
   */
  def fetchIfOlder(age: Long): InputStream = {
    if (isOlderThan(age)) {
      val remoteStream = remoteSource.fetchContent
      if (remoteStream != null) {
        //Save remote and fetch
        localFile.saveFromStream(remoteStream)
        localFile.loadToMemoryStream
      } else {
        //Fetch local copy
        localFile.loadToMemoryStream
      }
    } else {
      localFile.loadToMemoryStream
    }
  }

  /**
   * If file older than age milliseconds?
   * @param age Maximum age in milliseconds.
   */
  def isOlderThan(age: Long) = (localFile.getModificationTime < (System.currentTimeMillis() - age))
}