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
package vcc.domain.dndi

import java.io.InputStream
import xml.XML
import vcc.infra.datastore.naming.EntityID
import vcc.infra.datastore.{DataStoreIOException, DataStoreEntity}
import org.xml.sax.{SAXParseException, InputSource}

/**
 * Provides a way of loading DND4E files as DataStoreEntity.
 */
object CharacterBuilderImporter {
  def loadFromStream(is: InputStream): DataStoreEntity = {
    try {
      val xml = XML.load(new InputSource(is))

      if (xml.label != "D20Character") throw new DataStoreIOException("XML file does not represent a DND4E file", null)

      val cbo = new CharacterBuilderObject(xml)
      val map = cbo.getDatum()

      new DataStoreEntity(EntityID.fromName("dndi:character:" + map("base:name") + ":" + map("base:level")), map)
    } catch {
      case se: SAXParseException => throw new DataStoreIOException("Invalid XML file", se)
      case dse: DataStoreIOException => throw dse
      case e => throw new DataStoreIOException("Incomplete DND4E file", e)
    }
  }
}