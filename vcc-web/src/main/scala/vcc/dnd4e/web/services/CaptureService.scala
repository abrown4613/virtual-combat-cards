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
package vcc.dnd4e.web.services

import java.io.InputStream

trait CaptureService {

  def captureEntry(is: InputStream): Option[CaptureService.Result]
}

object CaptureService {
  sealed trait Result

  case class FailedCapture(clazz:String, id: Int) extends Result

  case class SuccessfulCapture(clazz:String, name: String, obj: AnyRef) extends Result

  private var service: CaptureService = null

  def setService(service: CaptureService) {
    this.service = service
  }

  def getInstance() = service
}
