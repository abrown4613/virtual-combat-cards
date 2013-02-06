/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e

import vcc.infra._
import vcc.infra.startup.StartupStep

import java.io.File
import vcc.infra.datastore.naming.DataStoreURI
import vcc.infra.datastore.DataStoreFactory
import java.net.URL
import java.lang.System
import java.util.UUID
import vcc.updater.UpdateManager

object Configuration extends AbstractConfiguration with StartupStep {
  private val metricIdentifierNotDefined = UUID.nameUUIDFromBytes("NOT DEFINED".getBytes("UTF8"))

  val autoStartWebServer = makeProperty[Boolean]("vcc.dnd4e.autoWebServer", "true", x => x == "true")
  val storeLogs = makeProperty[Boolean]("vcc.dnd4e.storeLogs", "false", x => x == "true")
  val baseDirectory = makeProperty[File]("vcc.dnd4e.basedir", System.getProperty("user.dir"), x => {
    new File(x)
  })
  //This is either the VM option (vcc.dnd4e.datadir), or default to InstallDirectory/"fs-wc"
  val dataDirectory: File = {
    if (System.getProperty("vcc.dnd4e.datadir") != null) new File(System.getProperty("vcc.dnd4e.datadir"))
    else new File(UpdateManager.getInstallDirectory, "fs-wc")
  }

  val compendiumStoreID = makeProperty[DataStoreURI]("vcc.dnd4e.compendium", null, x => {
    if (x != null) {
      expandDataStoreURI(DataStoreURI.fromStorageString(x))
    } else null
  })

  val metricIdentifier = makePropertyWithSerializer[Option[UUID]]("vcc.metric.identifier", "NOT DEFINED", value => {
    value match {
      case "NOT DEFINED" => Some(metricIdentifierNotDefined)
      case "NO" => None
      case s => try {
        Some(UUID.fromString(s))
      } catch {
        case e: IllegalArgumentException =>
          logger.warn("Unexpected UUID string: " + s)
          None
      }
    }
  }, value => {
    value match {
      case None => "NO"
      case Some(uuid) => uuid.toString
    }
  })

  def expandDataStoreURI(baseEsid: DataStoreURI): DataStoreURI = {
    if (baseEsid == null) return null
    val dsb = DataStoreFactory.getDataStoreBuilder(baseEsid)
    if (dsb != null) {
      if (dsb.isResolvedDataStoreURI(baseEsid))
        baseEsid
      else {
        logger.info("Configuration: Will expand ESID: {}", baseEsid)
        val modEsid = dsb.resolveDataStoreURI(baseEsid, Map("HOME" -> new File(System.getProperty("user.home")).toURI, "BASE" -> baseDirectory.value.toURI))
        logger.info("Configuration: Expanded to: {}", modEsid)
        modEsid
      }
    } else
      null
  }

  def createDirectoryTree(baseDir: java.io.File): Boolean = {
    if (baseDir.exists && !baseDir.isDirectory) {
      logger.error("{} exists but is a file, cannot complete configuration", baseDir)
      return false
    }
    if (!baseDir.exists) {
      logger.info("Have to create: {}", baseDir)
      if (!baseDir.mkdirs) {
        logger.warn("Failed to create directory: {}", baseDir)
        return false
      }
    }
    // Got hear means directory is ok
    true
  }

  def isStartupComplete = compendiumStoreID.value != null

  /**
   * String representation of the URL where VCC should search for updates
   */
  def getVersionReleaseURL = new URL(System.getProperty("vcc.update.url", "http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"))

  /**
   * This is used to figure out where the binaries are found on different platforms.
   * @see vcc.util.UpgradeManager.getInstallDirectory
   */
  def getInstallDirectory = new File(System.getProperty("vcc.install", System.getProperty("user.dir", ".")))

  /**
   * Maximum age of cached data.
   */
  val getCheckAfterAge: Long = {
    val timeout: Long = try {
      System.getProperty("vcc.update.check").toLong
    } catch {
      case _: NumberFormatException => 24 * 3600
    }
    timeout * 1000
  }

  def addMetricIdentifierIfNeeded(file: File) {
    if (metricIdentifier.value.isDefined && metricIdentifier.value.get == metricIdentifierNotDefined) {
      metricIdentifier.value = Some(UUID.randomUUID())
      save(file)
    }
  }
}