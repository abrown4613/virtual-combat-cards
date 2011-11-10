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
package vcc.infra.util

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import java.io.InputStream

class RemoteFileTest extends SpecificationWithJUnit with Mockito {

  trait context extends Scope {
    val mRemote = mock[RemoteFile.RemoteSource]
    val mLocal = mock[RemoteFile.LocalFile]
    val rf = new RemoteFile(mLocal, mRemote)
    val mInputStream = mock[InputStream]
    val mInputRemote = mock[InputStream]
  }

  "RemoteFile fetchIfOlder" should {
    "fetch local if age has not come" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 300)
      mLocal.loadToMemoryStream returns mInputStream

      val is = rf.fetchIfOlder(3600)
      is must_== mInputStream
      there was one(mLocal).getModificationTime
    }

    "return local if fetch failed" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 4000)
      mLocal.loadToMemoryStream returns mInputStream
      mRemote.fetchContent returns null

      val is = rf.fetchIfOlder(3600)
      is must_== mInputStream
      there was one(mLocal).getModificationTime then
        one(mRemote).fetchContent then
        one(mLocal).loadToMemoryStream
      there was no(mLocal).saveFromStream(any)
    }

    "return null if both local and remote failed" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 4000)
      mLocal.loadToMemoryStream returns null
      mRemote.fetchContent returns null

      val is = rf.fetchIfOlder(3600)

      is must beNull
      there was one(mLocal).getModificationTime then
        one(mRemote).fetchContent then
        one(mLocal).loadToMemoryStream
    }

    "return new data after saving local copy of fetched" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 4000)
      mLocal.loadToMemoryStream returns mInputRemote
      mRemote.fetchContent returns mInputRemote
      mLocal.saveFromStream(mInputRemote)

      val is = rf.fetchIfOlder(3600)

      is must_== mInputRemote

      there was one(mLocal).getModificationTime then
        one(mRemote).fetchContent then
        atLeastOne(mLocal).saveFromStream(mInputRemote) then       //TODO this is odd should be One
        one(mLocal).loadToMemoryStream

    }
  }
  "RemoteFile getLocalCopy" should {
    "fetch local if age has not come" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 300)
      mLocal.loadToMemoryStream returns mInputStream

      val is = rf.getLocalCopy
      is must_== mInputStream
      there was one(mLocal).loadToMemoryStream
    }
  }

  "RemoteFile isOlderThan" should {
    "be false if file is not older" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 300L)

      rf.isOlderThan(3600) must beFalse
      there was one(mLocal).getModificationTime
    }

    "be false if file is not older" in new context {
      mLocal.getModificationTime returns (System.currentTimeMillis() - 3602)

      rf.isOlderThan(3600) must beTrue
      there was one(mLocal).getModificationTime
    }

    "be true if file not present" in new context {
      mLocal.getModificationTime returns 0L

      rf.isOlderThan(3600) must beTrue
      there was one(mLocal).getModificationTime
    }
  }
}