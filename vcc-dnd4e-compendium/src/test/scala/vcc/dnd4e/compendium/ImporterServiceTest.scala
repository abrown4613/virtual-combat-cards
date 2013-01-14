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
package vcc.dnd4e.compendium

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import concurrent.SyncVar
import java.io.File
import vcc.advtools.MonsterReader

class ImporterServiceTest extends SpecificationWithJUnit {
  def is =
    "import works should return result" ! service().e1 ^
      "import works should return result" ! service().e2 ^
      "import fails should return none" ! service().e3 ^
      "provide import logic for dnd4e" ! service().e4 ^
      "provide import logic for monster" ! service().e5 ^
      "make proper compendium ID" ! service().makeProperCompendiumID ^
      "make proper non compendium ID" ! service().makeProperNonCompendiumID ^
      end

  case class service() extends Mockito {

    private val repository = mock[CompendiumRepository]
    private val service = new ImporterService(repository)

    def e1 = {
      val mResult = mock[CombatantEntity]
      val job = makeJob(Some(mResult))
      val barrier = new SyncVar[Option[CombatantEntity]]
      service.executeAndNotify(job, barrier.put(_))

      barrier.get(500).get must_== Some(mResult)
    }

    def e2 = {
      val mResult = mock[CombatantEntity]
      val job = makeJob(Some(mResult))
      val barrier = new SyncVar[Option[CombatantEntity]]
      service.executeAndNotify(job, barrier.put(_))
      there was one(repository).store(mResult)
    }

    def e3 = {
      val job = makeJob(None)
      val barrier = new SyncVar[Option[CombatantEntity]]
      service.executeAndNotify(job, barrier.put(_))

      barrier.get(500).get must_== None
    }

    def e4 = {
      val job = service.importJobForFile(new File("some.dnd4e"))
      (job.isDefined must beTrue) and (job.get.isInstanceOf[CharacterEntityReaderJob] must beTrue)
    }

    def e5 = {
      val job = service.importJobForFile(new File("some.monster"))
      (job.isDefined must beTrue) and (job.get.isInstanceOf[MonsterFileImportJob] must beTrue)
    }

    def makeProperCompendiumID = {
      val job = new MonsterFileImportJob(null)
      val reader = mock[MonsterReader]
      reader.getCompendiumID returns Some(10)
      job.makeBlankEntity(reader).eid must_== MonsterEntity.newInstance(10).eid
    }

    def makeProperNonCompendiumID = {
      val job = new MonsterFileImportJob(null)
      val reader = mock[MonsterReader]
      reader.getCompendiumID returns None
      reader.getContentDigest returns "Digest"
      job.makeBlankEntity(reader).eid must_== MonsterEntity.newInstance("Digest").eid
    }

    private def makeJob(result: Option[CombatantEntity]) = {
      val job = mock[ImportJob]
      job.execute() returns result
      job
    }
  }

}