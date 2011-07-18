/*
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
package vcc.infra.diskcache

import java.io.File

/**
 *
 */
trait UpdateableObjectStoreResolver[K, T] {
  def getObjectUpdateAwareLoader(key: K): UpdateAwareLoader[T]
}

/**
 *
 */
trait UpdateAwareLoader[T] {
  def getCurrent(): Option[T]
}

/**
 *
 */
class FileUpdateAwareLoader[T](val file: File, loader: File => Option[T]) extends UpdateAwareLoader[T] {
  private var modified = -1L
  private var lastValue: Option[T] = None

  def getCurrent(): Option[T] = {
    val newModified = file.lastModified()
    if (newModified != modified) {
      lastValue = loader(file)
      modified = newModified
    }
    lastValue
  }
}

/**
 * Allows fetching of object based on some external store that may be changed by other processes.
 * For example a files that may be updated by external editors.
 * @param resolver Should provide a valid ObjectStoreLoader for the give key.
 */
class UpdateableObjectStore[K, T](resolver: UpdateableObjectStoreResolver[K, T]) {
  private val cache = scala.collection.mutable.Map.empty[K, UpdateAwareLoader[T]]

  def fetch(key: K): Option[T] = {
    synchronized {
      if (!cache.isDefinedAt(key)) {
        val bo = resolver.getObjectUpdateAwareLoader(key)
        if (bo != null) cache += (key -> bo)
      }
      if (cache.isDefinedAt(key)) cache(key).getCurrent()
      else None
    }
  }
}
