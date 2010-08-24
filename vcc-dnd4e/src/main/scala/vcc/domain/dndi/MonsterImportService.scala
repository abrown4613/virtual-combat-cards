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

import vcc.model.Registry
import vcc.infra.datastore.naming._
import vcc.infra.fields._
import vcc.dnd4e.domain.compendium.{TrapEntity, Compendium, MonsterEntity}


/**
 * This service is used to get Monster form the DNDI Capture model into
 * the MonsterEntity model.
 */
object MonsterImportService {
  val logger = org.slf4j.LoggerFactory.getLogger("domain")

  //FIXME this is hardcoded for testing
  private val templateDirectory = new java.io.File("fs-wc/template")

  private final val defReformat = Set("stat:ac", "stat:reflex", "stat:will", "stat:fortitude")
  private final val reformatRE = """^\s*(\d+)\s*.*""".r

  def importObject(obj: DNDIObject) {
    obj match {
      case monster: Monster => importMonster(monster)
      case trap: Trap => importTrap(trap)
      case _ => logger.warn("Dont have import procedure for object class={}.", obj.clazz)
    }
  }

  def importTrap(dndiTrap: Trap) {
    // For this release traps will be Monsters
    val es = Compendium.activeRepository
    val trap = TrapEntity.newInstance(dndiTrap.id)

    logger.debug("Load D&DI Trap: {}", dndiTrap)
    processMonsterFieldSet(trap, dndiTrap)
    trap.trapClass.value = dndiTrap("base:type").get
    val template = CaptureTemplateEngine.fetchClassTemplate(dndiTrap.clazz, templateDirectory)
    val xml = template.render(dndiTrap)
    trap.statblock.value = xml.toString
    es.store(trap)
    logger.info("Imported DNDI trap {} as TrapEntity: {}", dndiTrap.id, trap.eid)
  }

  def importMonster(dndiMonster: Monster) {
    if (dndiMonster == null) return
    val es = Compendium.activeRepository
    val monster = MonsterEntity.newInstance(dndiMonster.id)
    logger.debug("Load D&DI Monster: {}", dndiMonster)
    processMonsterFieldSet(monster, dndiMonster)

    val template = CaptureTemplateEngine.fetchClassTemplate(dndiMonster.clazz, templateDirectory)
    val xml = template.render(dndiMonster)
    monster.statblock.value = xml.toString
    es.store(monster)
    logger.info("Imported DNDI monster {} as MonsterEntity: {}", dndiMonster.id, monster.eid)
  }

  private def processMonsterFieldSet(monster: MonsterEntity, dndiMonster: DNDIObject) = {
    val fieldMap = Map[String, Field[_]](
      "base:name" -> monster.name,
      "stat:hp" -> monster.hp,
      "stat:ac" -> monster.ac,
      "stat:reflex" -> monster.reflex,
      "stat:will" -> monster.will,
      "stat:fortitude" -> monster.fortitude,
      "base:role" -> monster.role,
      "base:xp" -> monster.xp,
      "stat:initiative" -> monster.initiative,
      "base:level" -> monster.level
      )
    for ((key, field) <- fieldMap) {
      val v = dndiMonster(key)
      try {
        if (v.isDefined) {
          val s = if (defReformat.contains(key)) {
            reformatRE.unapplySeq(v.get) match {
              case Some(List(mv)) => mv
              case _ => v.get
            }
          } else v.get
          field.fromStorageString(s)
        } else logger.warn("Mapping failed between {} and {}", key, field.id)
      } catch {
        case e =>
          logger.error("Exception while processing key " + key, e)
      }
    }
    fieldMap
  }
}
