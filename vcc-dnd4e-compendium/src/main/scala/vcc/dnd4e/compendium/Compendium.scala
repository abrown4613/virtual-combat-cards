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
package vcc.dnd4e.compendium

import vcc.infra.startup.StartupStep

object Compendium extends StartupStep {
  val monsterClassID = EntityClassID(new java.net.URI("vcc-class:monster"))
  val monsterClassIDStorageString = monsterClassID.uri.toString
  val characterClassID = EntityClassID(new java.net.URI("vcc-class:character"))
  val characterClassIDStorageString = characterClassID.uri.toString
  val trapClassID = EntityClassID(new java.net.URI("vcc-class:trap"))
  val trapClassIDStorageString = trapClassID.uri.toString

  private var _activeRepository:CompendiumRepository = null

  def setActiveRepository(es: CompendiumRepository) {
    _activeRepository = es
  }
  
  def activeRepository = _activeRepository
  
  def isStartupComplete = true 

}