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

import java.net.URL
import org.xml.sax.InputSource
import scala.xml.XML
import java.io.{InputStream, File}
import scala.swing.Frame
import swing.{MultiPanel, SwingHelper}
import java.awt.Image

/**
 * Utility functions to help with update manager
 */
object UpdateManager {

  /**
   * This is based on Drupal project versioning, we assume all version field are Int
   * except extra (which is a nullable string)
   */
  case class Version(major: Int, minor: Int, patch: Int, extra: String) extends Ordered[Version] {
    def compare(that: Version): Int = {
      var dif = this.major - that.major
      if (dif == 0) dif = this.minor - that.minor
      if (dif == 0) dif = this.patch - that.patch
      if (dif == 0)
        if (this.extra != null && that.extra != null) dif = this.extra.compare(that.extra)
        else if (this.extra != null) dif = -1
        else if (that.extra != null) dif = 1
      dif
    }

    def versionString: String = major + "." + minor + "." + patch + (if (extra != null) "-" + extra else "")

    def isPatch(other: Version): Boolean = {
      ((this.major == other.major) && (this.minor == other.minor) && this.patch > other.patch)
    }

    def isEligibleUpgradeFromVersion(fromVersion: Version): Boolean = {
      this > fromVersion && ((this.major == fromVersion.major && this.minor == fromVersion.minor) || this.patch == 0)
    }
  }

  val NotFoundVersion = Version(0, 0, 0, "NotFound")

  object Version {
    private val fullVersion = """(\d+)\.(\d+)\.(\d+)(\-\w+)?""".r
    private val partialVersion = """(\d+)\.(\d+)(\-\w+)?""".r

    /**
     * Load a version from a string
     */
    def fromString(str: String): Version = {
      str match {
        case fullVersion(major, minor, patch, qualif) =>
          val mqualif = if (qualif != null) qualif.substring(1) else null
          Version(major.toInt, minor.toInt, patch.toInt, mqualif)
        case partialVersion(major, minor, qualif) =>
          val mqualif = if (qualif != null) qualif.substring(1) else null
          Version(major.toInt, minor.toInt, 0, mqualif)
        case _ => null
      }
    }

    /**
     * Loads a version file and extract the version from it
     */
    def fromVersionFileFromStream(resource: InputStream): Version = {
      try {
        val vxml = XML.load(new InputSource(resource))
        if (vxml.label == "version") this.fromString(vxml.text) else NotFoundVersion
      } catch {
        case _ => NotFoundVersion
      }
    }
  }

  /**
   *  Case class containing basic information on a release, based on Drupal.org
   * release-history format
   * @param version A version string, e.g. 0.99.1
   * @param download URL to download file
   * @param md5 MD4 signature for file
   * @param info URL to release note
   */
  case class Release(version: Version, download: URL, md5: String, info: URL)

  /**
   * Collect version information from a stream, which should point to a XML file. This
   * can be a URL on a remote site. Only published versions should be returned.
   * @param stream The InputSource for the XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(stream: InputSource): Seq[Release] = {
    val release = XML.load(stream)
    val useUnpublished = System.getProperty("vcc.update.unpublished") != null

    val releases: Seq[Release] = (release \\ "release").map {
      release =>
        val major = XMLHelper.nodeSeq2Int(release \ "version_major", 0)
        val minor = XMLHelper.nodeSeq2Int(release \ "version_minor", 0)
        val patch = XMLHelper.nodeSeq2Int(release \ "version_patch", 0)
        val extra = XMLHelper.nodeSeq2String(release \ "version_extra", null)
        val download = XMLHelper.nodeSeq2String(release \ "download_link", null)
        val md5 = XMLHelper.nodeSeq2String(release \ "mdhash", null)
        val info = XMLHelper.nodeSeq2String(release \ "release_link", null)

        if ((!useUnpublished && XMLHelper.nodeSeq2String(release \ "status") != "published") || download == null) null
        else Release(
          Version(major, minor, patch, extra),
          if (download != null) new URL(download) else null,
          md5,
          if (info != null) new URL(info) else null
          )
    }.filter(r => r != null)
    assert(releases != null)
    releases
  }

  /**
   * Get version information based on an URL
   * @param URL to the site that contains a Drupal.org release-history XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(url: URL): Seq[Release] = {
    val stream = url.openStream()
    checkAvailableVersions(new InputSource(stream))
  }

  /**
   * Fetch available version then allow user to download choosen versiona 
   * and leave the version ready to update on next launch.
   * @param url URL to fecth available versions
   * @return Success or failure
   */
  def runUpgradeProcess(url: URL, currentVersion: Version, dialogIcon: Image) {

    import vcc.util.swing.multipanel.ReleaseSelectPanel

    /**
     * @return ( possible release, hasMore ) hasMore indicates that some newer versions where skipped because of full
     * upgrade policy.
     */
    def scanForVersions(afile: File): (List[(Symbol, Release)], Boolean) = {
      val rels = checkAvailableVersions(new InputSource(new java.io.FileInputStream(afile)))

      afile.delete()

      val possible = rels.filter(r => {r.version > currentVersion }).map({
        rel =>
          if (rel.version.isEligibleUpgradeFromVersion(currentVersion)) {
            if (rel.version.extra != null) ('RC, rel)
            else if (rel.version.isPatch(currentVersion)) ('PATCH, rel)
            else ('UPGRADE, rel)
          } else {
            ('NOTALLOWED, rel)
          }
      }).toList
      (possible.filter(x => x._1 != 'NOTALLOWED), possible.exists(x => x._1 == 'NOTALLOWED))
    }

    def checkFileMD5Sum(file: File, md5sum: String): Boolean = {
      val chkSum = PackageUtil.fileMD5Sum(file)
      md5sum.toLowerCase == chkSum.toLowerCase
    }
    val umd = new Frame() with MultiPanel {
      title = "Update Virtual Combat Cards"
      minimumSize = new java.awt.Dimension(300, 200)
      iconImage = dialogIcon
    }
    umd.visible = true

    umd.showMessage(false, "Checking for a new version...")

    val afile = umd.downloadFile(url, File.createTempFile("vcc", ".xml"))
    if (afile != null) {
      val (releases, hasMore) = scanForVersions(afile)
      if (releases.length > 0) {
        val releaseOpt = umd.customPanel(new ReleaseSelectPanel(releases, hasMore))

        if (releaseOpt.isDefined) {
          val release = releaseOpt.get

          val dfile = umd.downloadFile(release.download, java.io.File.createTempFile("vcc", ".zip"))
          if (dfile != null) {
            umd.showMessage(false, "Checking and unpacking downloaded file...")
            // We have the file
            if (checkFileMD5Sum(dfile, release.md5)) {
              PackageUtil.extractFilesFromZip(dfile, getInstallDirectory)
              umd.showMessage(true, "<html><body>Download and extraction completed successfully.<p>To update Virtual Combat Cards, exit and restart it.</body></html>")
            } else {
              umd.showMessage(true, "<html><body>Downloaded file seems to be corrupted. <p> Download and extract manually or report a bug on the site</body></html>")
              SwingHelper.openDesktopBrowser(release.info)
            }
          } else {
            umd.showMessage(true, "Download failed or cancelled.")
          }
        } else {
          // No version selected
        }
      } else {
        umd.showMessage(true, "Your version is up to date.")
      }
    } else {
      umd.showMessage(true, "<html><body>Failed to download Releases history from:<br>" + url + "</body></html>")
    }
    umd.dispose()
  }

  /**
   * Returns a File which points to the directory in which VCC Binary installation is
   * located.
   * @returm File The VCC install directory
   */
  def getInstallDirectory(): File = new File(System.getProperty("vcc.install", System.getProperty("user.dir", ".")))

}