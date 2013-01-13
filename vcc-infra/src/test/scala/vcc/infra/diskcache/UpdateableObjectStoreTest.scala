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
package vcc.infra.diskcache

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import java.io.File

class UpdateableObjectStoreTest extends SpecificationWithJUnit with Mockito {

  trait commonMock extends Scope {
    val mockResolver = mock[UpdateableObjectStoreResolver[Int, String]]
    val mockLoader = mock[UpdateAwareLoader[String]]
    val store = new UpdateableObjectStore[Int, String](mockResolver)
  }

  "UpdateableObjectStore" should {

    "ask resolver for object if its not already known" in new commonMock {
      mockResolver.getObjectUpdateAwareLoader(10) returns mockLoader
      mockLoader.getCurrent() returns Some("Ten")
      store.fetch(10) must_== Some("Ten")
      there was one(mockResolver).getObjectUpdateAwareLoader(10) andThen
              one(mockLoader).getCurrent()

    }
    "call already know object when it is already defined" in new commonMock {
      mockResolver.getObjectUpdateAwareLoader(10) returns mockLoader
      mockLoader.getCurrent() returns Some("Ten") thenReturns Some("Eleven")
      store.fetch(10) must_== Some("Ten")
      there was one(mockResolver).getObjectUpdateAwareLoader(10)
      there was one(mockLoader).getCurrent()
      store.fetch(10) must_== Some("Eleven")
      there was one(mockResolver).getObjectUpdateAwareLoader(10)
      there was two(mockLoader).getCurrent()

    }
    "null object are not registered, and will be return as null" in new commonMock {
      mockResolver.getObjectUpdateAwareLoader(10) returns null
      store.fetch(10) must_== None
      there was one(mockResolver).getObjectUpdateAwareLoader(10)
      store.fetch(10) must_== None
      there was two(mockResolver).getObjectUpdateAwareLoader(10)
    }
  }

  trait fileUpdateMock extends Scope {
    val mockFile = mock[File]
    val mockLoader = mock[File => Option[Int]]
    val fileLoader = new FileUpdateAwareLoader(mockFile, mockLoader)
  }

  "FileUpdateAwareLoader" should {

    "return object, getting the last modification date" in new fileUpdateMock {
      mockLoader.apply(mockFile) returns Some(10)
      fileLoader.getCurrent() must_== Some(10)
      there was one(mockFile).lastModified
      there was one(mockLoader).apply(mockFile)
    }

    "check time on every evocation" in new fileUpdateMock {
      mockLoader.apply(mockFile) returns Some(10)
      fileLoader.getCurrent() must_== Some(10)
      there was one(mockFile).lastModified
      fileLoader.getCurrent() must_== Some(10)
      there was two(mockFile).lastModified
    }

    "check time on every evocation, but not call loader every time" in new fileUpdateMock {
      mockLoader.apply(mockFile) returns Some(10)
      fileLoader.getCurrent() must_== Some(10)
      there was one(mockFile).lastModified
      fileLoader.getCurrent() must_== Some(10)
      there was two(mockFile).lastModified
      there was one(mockLoader).apply(mockFile)
    }

    "if time changed, call loader again" in new fileUpdateMock {
      mockFile.lastModified returns 10 thenReturns 9 // Move back should trigger update
      mockLoader.apply(mockFile) returns Some(10) thenReturns Some(20)
      fileLoader.getCurrent() must_== Some(10)
      there was one(mockFile).lastModified
      there was one(mockLoader).apply(mockFile)
      fileLoader.getCurrent() must_== Some(20)
      there was two(mockFile).lastModified
      there was two(mockLoader).apply(mockFile)
    }
  }
}