/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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

import java.io.{FileInputStream, File}
import vcc.util.{AsynchronousTask, AsynchronousDispatcher}
import vcc.dndi.reader.CharacterBuilderImporter
import org.slf4j.LoggerFactory
import vcc.advtools.MonsterReader

class ImporterService(val repository: CompendiumRepository) extends Importer with AsynchronousDispatcher.Observer[CombatantEntity] {

  private val taskManager = new AsynchronousDispatcher[CombatantEntity]()
  taskManager.setObserver(this)

  private class ImportTask(job: ImportJob, val callback: (Option[CombatantEntity]) => Unit) extends AsynchronousTask[CombatantEntity] {
    def execute(): CombatantEntity = job.execute().get
  }

  def importJobForFile(file: File): Option[ImportJob] = {
    if (file.getAbsolutePath.endsWith(".dnd4e"))
      Some(new CharacterEntityReaderJob(file))
    else if (file.getAbsolutePath.endsWith(".monster"))
      Some(new MonsterFileImportJob(file))
    else
      None
  }

  def executeAndNotify(job: ImportJob, callback: (Option[CombatantEntity]) => Unit) {
    taskManager.queueTasks(Seq(new ImportTask(job, callback)))
  }

  def taskComplete(task: AsynchronousTask[CombatantEntity], result: CombatantEntity) {
    repository.store(result)
    task.asInstanceOf[ImportTask].callback(Some(result))
  }

  def taskFailed(task: AsynchronousTask[CombatantEntity], error: Throwable) {
    task.asInstanceOf[ImportTask].callback(None)
  }
}

class CharacterEntityReaderJob(file: File) extends ImportJob {
  def execute(): Option[CombatantEntity] = {
    try {
      val is = new FileInputStream(file)
      val dse = CharacterBuilderImporter.loadFromStream(is)
      val ent = CombatantEntityBuilder.buildEntity(dse)
      Some(ent)
    } catch {
      case e =>
        LoggerFactory.getLogger("domain").error("Failed import of Character", e)
        None
    }
  }
}

class MonsterFileImportJob(file: File) extends ImportJob {

  def execute(): Option[CombatantEntity] = {
    val mr = new MonsterReader(new FileInputStream(file))
    val ent = makeBlankEntity(mr)

    loadFromReader(ent, mr)
    Some(ent)
  }

  private[compendium] def makeBlankEntity(reader: MonsterReader): MonsterEntity = {
    if (reader.getCompendiumID.isDefined)
      MonsterEntity.newInstance(reader.getCompendiumID.get)
    else
      MonsterEntity.newInstance(reader.getContentDigest)
  }

  private def loadFromReader(entity: MonsterEntity, reader: MonsterReader) {
    entity.name.value = reader.getName
    val category = reader.getGroupCategory
    val role = category.groupRole
    entity.role.value = normalizeRole(role) + " " + category.role + (if(category.isLeader) " (Leader)" else "")
    entity.level.value = category.level
    entity.xp.value = category.experience

    val stats = reader.getBaseStats
    entity.initiative.value = stats.initiative
    entity.hp.value = stats.hitPoint

    val defense = reader.getDefense
    entity.ac.value = defense.ac
    entity.fortitude.value = defense.fortitude
    entity.reflex.value = defense.reflex
    entity.will.value = defense.will
  }

  private def normalizeRole(role:String) = role match {
    case "Standard" => ""
    case s => s
  }
}