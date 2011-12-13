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
package vcc.dnd4e.model

import scala.xml._
import vcc.infra.datastore.naming._
import vcc.util.XMLHelper._
import vcc.dnd4e.tracker.common.CombatantID
import java.io.{File, InputStream}

/**
 * Represents a party member, with an EntityID and options alias and ID
 * @param id Optional combatant id, null if not present
 * @param alias Optional combatant alias, null if not present
 * @param eid Combatant EntityID this is mandatory.
 */
case class PartyMember(id: CombatantID, alias: String, eid: EntityID) {
  def toXML = (<combatant eid={eid.uri.toString} alias={alias} id={if (id != null) id.id else null}/>)
}

/**
 * Load a Party of PEML file.
 */
object PartyFile {
  private val logger = org.slf4j.LoggerFactory.getLogger("domain")

  private def parseEntry(node: Node): PartyMember = {
    val eidUri = nodeSeq2String(node \ "@eid")
    val id = nodeSeq2String(node \ "@id", null)
    val alias = nodeSeq2String(node \ "@alias", null)
    val eid = EntityID.fromStorageString(eidUri)
    
    val combatantID:CombatantID = if(CombatantID.isValidID(id)) CombatantID(id) else null

    PartyMember(combatantID, alias, eid)
  }

  private def loadFromXML(node: Node): (List[PartyMember], Boolean) = {
    var x: List[PartyMember] = Nil
    var ok = true
    val ver = {
      val vnl = node \ "@version"
      if (vnl.isEmpty) None
      else Some(vnl(0).text)
    }
    if (node.label == "party" && ver == Some("1.0")) {
      for (snode <- node.child if (snode.label != "#PCDATA")) {
        try {
          x = parseEntry(snode) :: x
        } catch {
          case e: Exception =>
            logger.warn("Failed to load node: " + snode, e)
            ok = false
        }
      }
    } else {
      if (ver == None) logger.error("No version found on file, this may be a legacy file")
      else logger.error("Failed to load party, either is not a party")
      ok = false
    }
    val (withId, noId) = x.reverse.partition(pm => pm.id != null)
    (withId ++ noId, ok)
  }

  /**
   * Load a PEML party and return a list of PartyMembers
   * @param Stream InputStream containing the XML for the PartyFile
   * @return A pair with the loadable PartyMember and a OK flag. If ok = false, there
   * was some error processing the file. In out is (Nil,false) the file is invalid.
   * (Nil,true) means the file is empty. And some element and false means that not all
   * element in the file were read
   */
  def loadFromStream(stream: InputStream): (List[PartyMember], Boolean) = {
    try {
      val node = XML.load(stream)
      loadFromXML(node)
    } catch {
      case e =>
        logger.error("Failed to load: " + e.getMessage, e)
        (Nil, false)
    }
  }

  /**
   * Saves a sequence for PartyMembers to a XML file.
   * @param file The file to be saved
   * @param entries Entries to save
   */
  def saveToFile(file: File, entries: Seq[PartyMember]) {
    val doc = (<party version='1.0'>
      {entries.map(_.toXML)}
    </party>)
    XML.save(file.toString, doc, "UTF-8", true, null)
  }
}