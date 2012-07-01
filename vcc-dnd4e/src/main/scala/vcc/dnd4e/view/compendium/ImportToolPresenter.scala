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

import vcc.infra.datastore.naming.EntityID
import java.io.File
import vcc.dnd4e.view.compendium.ImportToolView.UserView
import vcc.dnd4e.compendium.{Importer, CombatantEntity}

class ImportToolPresenter(importer: Importer) extends ImportToolView.Presenter {
  private var view: ImportToolView.UserView = null

  private var pendingJobs = 0
  private var completeJobs = 0
  private var imported: List[(EntityID, String)] = Nil

  def registerView(view: UserView) {
    this.view = view
  }

  def processFiles(files: Seq[File]) {
    val jobs = files.flatMap(importer.importJobForFile)
    for (job <- jobs) {
        importer.executeAndNotify(job, taskComplete)
    }
    if (!jobs.isEmpty) {
      pendingJobs = pendingJobs + jobs.size
      view.updateProgress(completeJobs, pendingJobs)
    }
  }

  def selectStatBlock(eid: EntityID) {
  }

  private def taskComplete(resultOption: Option[CombatantEntity]) {
    completeTask()

    resultOption.map {
      result =>
        imported = imported ::: List((result.eid, result.name.value))
        view.setListContent(imported)
    }
  }

  private def completeTask() {
    completeJobs = completeJobs + 1
    if (completeJobs == pendingJobs) {
      view.updateProgress(0, 0)
      completeJobs = 0
      pendingJobs = 0
    } else
      view.updateProgress(completeJobs, pendingJobs)
  }
}