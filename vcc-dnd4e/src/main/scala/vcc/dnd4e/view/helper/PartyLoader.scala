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
package vcc.dnd4e.view.helper

import vcc.dnd4e.model.{PartyFile, PartyMember}
import vcc.dnd4e.tracker.common.Command.AddCombatants
import vcc.dnd4e.domain.compendium.CompendiumRepository
import vcc.model.Registry
import vcc.infra.datastore.naming._
import vcc.dnd4e.model.CombatantEntityBuilder

import scala.swing.{Dialog, Component}
import vcc.dnd4e.view.PanelDirector
import vcc.dnd4e.tracker.common.CombatantRosterDefinition
import java.io.{FileInputStream, File}
import vcc.dnd4e.tracker.common.CombatantEntity

/**
 * Companion object to the PartyLoader.
 */
object PartyLoader {
  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  /**
   * Provides an instance of a PartyLoader backed by some UI components
   * @param director PanelDirector to receive the load actions
   * @param owner Component that will own the Dialog popups.
   * @return A PartyLoader
   */
  def getInstance(director: PanelDirector, owner: Component): PartyLoader = {
    val esid = Registry.get[DataStoreURI]("Compendium").get
    val es = Registry.get[CompendiumRepository](esid).get

    val loader = new PartyLoader(es, new ViewPeer {
      def showError(title: String, message: String) {
        Dialog.showMessage(owner, message, title, Dialog.Message.Error, null)
      }

      def addCombatants(combatants: List[CombatantRosterDefinition]) {
        logger.debug("Sending request to tracker: {}", AddCombatants(combatants))
        director requestAction AddCombatants(combatants)
      }

      def confirm(title: String, message: String): Boolean = {
        Dialog.showConfirmation(owner, message, title, Dialog.Options.YesNo) == Dialog.Result.Yes
      }
    })
    loader
  }

  trait ViewPeer {
    def confirm(title: String, message: String): Boolean

    def addCombatants(combatants: List[CombatantRosterDefinition])

    def showError(title: String, message: String)
  }

}

/**
 * PartyLoader helper that handles loading and validation party files.
 */
class PartyLoader private[helper](es: CompendiumRepository, peer: PartyLoader.ViewPeer) {

  /**
   * Parse an Party File, validates and load to the battle.
   * @param file Party file
   */
  def loadToBattle(file: File) {
    val res = PartyFile.loadFromStream(new FileInputStream(file))
    val lst = validatePartyLoadAndWarn(res)
    if (!lst.isEmpty) loadToBattle(lst)
  }

  /**
   * Provides visual translation to error in loading the Party File.
   * @param pair The result of PartyFile.loadFromStream
   * @return The party list on a normal load. If partial load is triggered, and user confirms load, the salvaged entries.
   */
  def validatePartyLoadAndWarn(pair: (Seq[PartyMember], Boolean)): Seq[PartyMember] = {
    pair match {
      case (Nil, false) =>
        peer.showError("Invalid Party File", "File is not a valid Party File. Check vcc.log for details.")
        Nil
      case (Nil, true) =>
        peer.showError("Empty Party File", "The party files contains no entries.")
        Nil
      case (lst, false) =>
        if (peer.confirm("Invalid Entries in Party File", "Not all entries in the party file could be processed.\nDo you wish to proceed with only the valid entries?"))
          lst
        else
          Nil
      case (lst, true) =>
        lst
    }
  }

  /**
   * Load combatants form a PartyFile to the battle.
   * @param members Party member to try to load.
   */
  def loadToBattle(members: Seq[PartyMember]) {
    val cd = resolveEntries(members)
    if (!cd.isEmpty) peer.addCombatants(cd)
  }

  /**
   * Maps EntityID to the CombatantRosterDefinition by loading from the CompendiumRepository.
   * @param members Party members to the loaded.
   * @return Party members successfully resolved. If not all are found, they user is prompted for an option to
   * abort the load.
   */
  private[helper] def resolveEntries(members: Seq[PartyMember]): List[CombatantRosterDefinition] = {
    //First load an map all entities to definitions
    val cem = scala.collection.mutable.Map.empty[EntityID, CombatantEntity]
    for (member <- members) {
      if (!cem.isDefinedAt(member.eid)) {
        val ent = es.load(member.eid, true)
        if (ent != null) {
          cem += (member.eid -> CombatantEntityBuilder.fromCompendiumCombatantEntity(ent))
        }
      }
    }

    val cds: Seq[CombatantRosterDefinition] = for (pm <- members) yield {
      if (cem.isDefinedAt(pm.eid)) {
        CombatantRosterDefinition(pm.id, pm.alias, cem(pm.eid))
      } else null
    }

    val filtered = cds.filter(x => x != null)
    if (filtered.length == cds.length ||
      peer.confirm("Missing Combatants",
        "Not all combatants in the party where found in the compendium.\nDo you wish to load the ones that were found?")
    ) {
      filtered.toList
    } else
      Nil
  }
}