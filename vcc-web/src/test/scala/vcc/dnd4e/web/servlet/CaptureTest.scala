/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.web.servlet

import org.scalatra.test.specs2.MutableScalatraSpec
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import vcc.dnd4e.web.services.CaptureService
import java.io.InputStream
import vcc.dnd4e.web.services.CaptureService.{FailedCapture, SuccessfulCapture, Result}

@RunWith(classOf[JUnitRunner])
class CaptureTest extends MutableScalatraSpec {
  args(sequential = true)

  CaptureService.setService(mockService)

  addServlet(classOf[CaptureServlet], "/capture")

  "Information on Get" should {
    "return warning on get" in {
      get("/capture") {
        status must_== 200
        body must contain("This page should be used with the D&D Insider Capture plugin")
      }
    }

    "return true on a has=reply-text parameter" in {
      get("/capture?has=" + "reply-text") {
        status must_== 200
        body must_== "true"
      }
    }

    "return false if has is not correct option" in {
      get("/capture?has=" + "foo") {
        status must_== 200
        body must_== "false"
      }
    }
  }

  "Capture on Post" should {
    "post wrong content should produce error" in {
      post("/capture?reply=plugin-text", body = "badIntput".getBytes("UTF-8")) {
        status must_== 200
        body.stripLineEnd must_== "FATAL: Bad Request."
      }
    }

    "post new trap should read" in {
      post("/capture?reply=plugin-text", body = "goodCapture".getBytes("UTF-8")) {
        status must_== 200
        body.stripLineEnd must_== "Success: bob (dudeClass)"
      }
    }

    "post a monster should work" in {
      post("/capture?reply=plugin-text", body = "unsupported".getBytes("UTF-8")) {
        status must_== 200
        body.stripLineEnd must_== "Error: Failed capture of 'unsupportedClass' with id=111."
      }
    }

    "post unknown id" in {
      post("/capture?reply=plugin-text", body = "unknown".getBytes("UTF-8")) {
        status must_== 200
        body.stripLineEnd must_== "FATAL: Unknown entry type 'unknownClass'"
      }
    }
  }

  private object mockService extends CaptureService {
    def captureEntry(is: InputStream): Option[Result] = {
      getContent(is) match {
        case "badInput" => None
        case "goodCapture" => Some(SuccessfulCapture("dudeClass", "bob", null))
        case "unsupported" => Some(FailedCapture("unsupportedClass", 111))
        case "unknown" => Some(FailedCapture("unknownClass", -1))
        case _ => None
      }
    }

    private def getContent(is: InputStream): String = {
      scala.io.Source.fromInputStream(is).getLines().mkString("\n")
    }
  }
}