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

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.compendium._
import org.mockito.Mockito._

class MockedCompendium {

  private val monsterEntries = createMonsterEntries(10)
  private val trapEntries = createTrapEntries(10)
  private val characterEntries = createCharacterEntries(10)

  def initialize() {
    val mockRepository = createMockCompendium()
    Compendium.setActiveRepository(mockRepository)
  }


  private def createMockCompendium(): CompendiumRepository = {
    val mockRepository: CompendiumRepository = mock(classOf[CompendiumRepository])
    when(mockRepository.getMonsterSummaries).thenReturn(monsterEntries)
    when(mockRepository.getCharacterSummaries).thenReturn(characterEntries)
    when(mockRepository.getTrapSummaries).thenReturn(trapEntries)

    mockFetch(mockRepository, monsterEntries)
    mockFetch(mockRepository, trapEntries)
    mockFetch(mockRepository, characterEntries)

    mockFullMonster(mockRepository, monsterEntries)
    mockRepository
  }

  private def mockFetch(mockRepository: CompendiumRepository, entries: Seq[EntitySummary]) {
    entries.foreach(entry => {
      when(mockRepository.getEntitySummary(entry.eid)).thenReturn(entry)
      when(mockRepository.containsEntity(entry.eid)).thenReturn(true)
    })
  }

  private def mockFullMonster(mockRepository: CompendiumRepository, monsters: Seq[MonsterSummary]) {
    monsters.foreach(monster => {
      val m = new MonsterEntity(monster.eid)
      m.hp.value = 1
      m.statblock.value = "dummy"
      m.name.value = monster.name
      m.initiative.value = 1
      when(mockRepository.load(monster.eid, true)).thenReturn(m)
    })
  }

  private def createTrapEntries(maxLevel: Int): Seq[TrapSummary] = {
    (1 to maxLevel).map {
      level =>
        TrapSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Trap " + level, level, level * 100, "Trap", false)
    }
  }

  private def createCharacterEntries(maxLevel: Int): Seq[CharacterSummary] = {
    (1 to maxLevel).map {
      level =>
        CharacterSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Character " + level, level, "Human", "Fighter")
    }
  }

  private def createMonsterEntries(maxLevel: Int): Seq[MonsterSummary] = {
    (1 to maxLevel).map {
      level =>
        MonsterSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Monster " + level, level, level * 100, "soldier", false)
    }
  }
}