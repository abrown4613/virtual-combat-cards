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
package vcc.dnd4e.view.compendium

import org.specs2.mutable.SpecificationWithJUnit
import java.io.File
import org.specs2.mock.Mockito
import vcc.dnd4e.compendium.{ImportJob, Importer, MonsterEntity, CombatantEntity}
import collection.immutable.Queue
import org.specs2.specification.Scope
import vcc.infra.datastore.naming.EntityID
import org.w3c.dom.Document

class ImportToolPresenterTest extends SpecificationWithJUnit with Mockito {

  trait env extends Scope {
    val view = spy(new MockView)
    val importer = spy(new MockImporter())
    val file1 = new File("albert.dnd4e")
    val entity1 = makeEntity("Albert")
    val file2 = new File("batman.monster")
    val entity2 = makeEntity("Batman")
    val file3 = new File("joker.monster")
    val entity3 = makeEntity("Joker")
    val fileBad = new File("not.good")
    val fileWillFail = new File("will-not.load")
    val presenter = new ImportToolPresenter(importer)
    val expectedEntities = List(entity1, entity2, entity3)

    importer.registerTaskResult(file1, entity1)
    importer.registerTaskResult(file2, entity2)
    importer.registerTaskResult(file3, entity3)
    importer.registerTaskBadResult(fileWillFail)
    presenter.registerView(view)

    protected def entityView(count: Int) = expectedEntities.take(count).map(e => (e.eid, e.name.value))
  }

  "ImportToolPresenter" should {

    "handle single file import" in new env {
      presenter.processFiles(Seq(file1))

      there was one(importer).importJobForFile(file1) then
        one(view).updateProgress(0, 1)

      importer.finishFirst()

      there was one(view).updateProgress(0, 0) then
        one(view).setListContent(List((entity1.eid, entity1.name.value)))
    }

    "handle multiple file import" in new env {
      presenter.processFiles(Seq(file1, file2))

      there was one(importer).importJobForFile(file1) then
        one(importer).importJobForFile(file2)

      there was one(view).updateProgress(0, 2)

      importer.finishFirst()

      there was one(view).updateProgress(1, 2) then
        one(view).setListContent(entityView(1))

      importer.finishFirst()

      there was one(view).setListContent(entityView(2))
    }

    "add 2 files, finish one, add 1, finish" in new env {
      val expectedProgress = List((0, 2), (1, 2), (1, 3), (2, 3), (0, 0))
      presenter.processFiles(Seq(file1, file2))

      there was one(importer).importJobForFile(file1) then
        one(importer).importJobForFile(file2)

      view.progress must_== expectedProgress.take(1)

      importer.finishFirst()

      view.progress must_== expectedProgress.take(2)
      there was one(view).setListContent(entityView(1))

      presenter.processFiles(Seq(file3))
      view.progress must_== expectedProgress.take(3)

      importer.finishFirst()

      view.progress must_== expectedProgress.take(4)
      there was one(view).setListContent(entityView(2))

      importer.finishFirst()
      view.progress must_== expectedProgress
    }

    "have correct progress when add one file, finish one, add 1, finish" in new env {
      presenter.processFiles(Seq(file1))
      there was one(view).updateProgress(0, 1)
      importer.finishFirst()
      presenter.processFiles(Seq(file2))
      there was one(view).updateProgress(0, 0) then
        two(view).updateProgress(0, 1)

      importer.finishFirst()
      view.progress must_== List((0, 1), (0, 0), (0, 1), (0, 0))
    }

    "not attempt to import unsupported file" in new env {
      presenter.processFiles(Seq(fileBad))
      there was no(view).updateProgress(any, any)
    }

    "not attempt to import unsupported file and good" in new env {
      presenter.processFiles(Seq(fileBad, file1))
      there was one(view).updateProgress(0, 1)
    }

    "import only file but report all processed" in new env {
      presenter.processFiles(Seq(fileWillFail, file1))
      importer.finishFirst()
      importer.finishFirst()
      view.progress must_== List((0, 2), (1, 2), (0, 0))
    }

    "import only file but report all processed last bad" in new env {
      presenter.processFiles(Seq(file1, fileWillFail))
      importer.finishFirst()
      importer.finishFirst()
      view.progress must_== List((0, 2), (1, 2), (0, 0))
    }
  }

  private def makeEntity(name: String) = {
    val ent = MonsterEntity.newInstance()
    ent.loadFromMap(Map(
      "base:name" -> name, "stat:initiative" -> "2", "stat:hp" -> "30",
      "base:role" -> "Soldier", "base:level" -> "4", "base:xp" -> "600"
    ))
    ent
  }

  class MockImporter extends Importer with Mockito {

    private var results = Map.empty[File, Option[CombatantEntity]]
    private var queue = Queue.empty[(ImportJob, Option[CombatantEntity] => Unit)]

    def registerTaskResult(file: File, result: CombatantEntity) {
      results = results + (file -> Some(result))
    }

    def registerTaskBadResult(file: File) {
      results = results + (file -> None)
    }

    def finishFirst() {
      val ((job, callback), newQueue) = queue.dequeue
      callback(job.execute())
      queue = newQueue
    }

    def importJobForFile(file: File): Option[ImportJob] = {
      results.get(file).map {
        r =>
          val job = mock[ImportJob]
          job.execute() returns r
          job
      }
    }

    def executeAndNotify(job: ImportJob, callback: (Option[CombatantEntity]) => Unit) {
      queue = queue.enqueue((job, callback))
    }
  }

  class MockView extends ImportToolView.UserView {
    private var progressEvents: List[(Int, Int)] = Nil

    def setStatBlock(document: Document) {}

    def updateProgress(done: Int, total: Int) {
      progressEvents = (done, total) :: progressEvents
    }

    def setListContent(content: List[(EntityID, String)]) {}

    def progress = progressEvents.reverse
  }

}