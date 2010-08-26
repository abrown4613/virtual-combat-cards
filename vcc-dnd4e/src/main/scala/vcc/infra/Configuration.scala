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
package vcc.infra

import java.io._
import java.util.Properties

abstract class AbstractConfiguration {
  val logger = org.slf4j.LoggerFactory.getLogger("startup")

  private val props = new Properties()
  private var propMap = Map.empty[String, Property[_]]

  protected class Property[T](propName: String, propDefault: String, conv: String => T) {
    private var _value: T = {
      if (propDefault != null) props.setProperty(propName, propDefault)
      conv(propDefault)
    }

    def fromString(v: String) {
      value = conv(v)
    }

    def value_=(v: T) {
      props.setProperty(propName, v.toString)
      _value = v
    }

    def value: T = _value

  }

  def Property[T](propName: String, propDefault: String, conv: String => T): Property[T] = {
    if (propMap.isDefinedAt(propName))
      vcc.infra.AbnormalEnd(this, "Attempting to redefine property " + propName)
    val p = new Property[T](propName, propDefault, conv)
    propMap = propMap + (propName -> p)
    p
  }

  def load(file: File) {
    logger.info("Loading configuration file: " + file)
    props.loadFromXML(new FileInputStream(file))
    val enum = props.propertyNames.asInstanceOf[java.util.Enumeration[String]]
    while (enum.hasMoreElements) {
      val key = enum.nextElement
      if (propMap.isDefinedAt(key)) {
        try {
          propMap(key).fromString(props.getProperty(key))
          logger.debug("Loaded configuration parameter: {} with {}", key, props.getProperty(key))
        } catch {
          case e =>
            logger.warn("Could not read property: {} with value: {}", Array(key, props.getProperty(key)), e)
        }
      } else {
        logger.warn("Ignoring property: " + key)
      }
    }
  }

  def save(file: File) {
    logger.info("Saving to configuration file: " + file)
    props.storeToXML(new FileOutputStream(file), " Virtual Combat Cards Configution File")
    logger.debug("Saving to configuration file complete")
  }

  def dumpToLog() {
    for ((k, v) <- propMap) {
      logger.debug("Configuration property [{}]={}", k, v.value)
    }
  }
}



object ConfigurationFinder {
  private val searchVars = List("vcc.home", "user.dir", "user.home")

  val configFilename = "vcc.properties"

  def locateFile(): java.io.File = {
    for (ev <- searchVars) {
      val prop = System.getProperty(ev)
      if (prop != null) {
        var file = new File(prop, configFilename)
        if (file.exists && file.isFile && file.canRead) return file
      }
    }
    null
  }

  def foundConfiguration = locateFile != null

}
