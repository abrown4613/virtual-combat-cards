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
package org.exnebula.metric

import org.specs2.SpecificationWithJUnit

class MetricReporterTest extends SpecificationWithJUnit {
  def is =
    "have proper UUID" ! checkUUID ^
    "send a proper request" ! exampleGood ^
    "send a bad request" ! exampleBad ^
    "throw exception if something is really wrong" ! exampleReallyBad ^
    "build proper file" ! buildFormat ^
    end


  def checkUUID = {
    MetricTestHelper.testUUID.toString must_== "b68effa8-0121-3c8f-8d2d-c50e702fd24c"
  }

  def buildFormat = {
    MetricReporter.buildMessage(MetricTestHelper.testUUID, Map("some" -> "10", "another.bc" -> "abc")) must_==
      (<sample id={MetricTestHelper.testUUID.toString}><metric id="some" value="10" /><metric id="another.bc" value="abc" /></sample>)
  }
  def exampleGood = {
    val xml = (<sample id={MetricTestHelper.testUUID.toString}><metric id="some" value="10" /></sample>)
    MetricReporter.sendMetric(xml) must_== (200, "OK")
  }

  def exampleBad = {
    val xml = (<sample><metric id="some" value="10" /></sample>)
    MetricReporter.sendMetric(xml) must_== (400, "Missing UUID")
  }

  def exampleReallyBad = {
    MetricReporter.sendMetric(null) must throwA[NullPointerException]
  }
}