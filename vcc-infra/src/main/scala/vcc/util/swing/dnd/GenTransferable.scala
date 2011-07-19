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
//$Id$
package vcc.util.swing.dnd

import java.awt.datatransfer.{DataFlavor, Transferable}

/**
 * Generic Transferable
 */
class GenTransferable[T <: AnyRef](obj: T, flavor: DataFlavor, description: String) extends Transferable {

  def this(obj: T) = this (obj, GenTransferable.makeFlavor(obj.getClass), null)

  def this(obj: T, description: String) = this (obj, GenTransferable.makeFlavor(obj.getClass), description)

  def getTransferDataFlavors: Array[DataFlavor] = Array(flavor)

  def isDataFlavorSupported(flavor: DataFlavor): Boolean = getTransferDataFlavors.contains(flavor)

  def getTransferData(flavor: DataFlavor): AnyRef = {
    if (this.flavor == flavor) {
      obj
    } else null
  }

  def getDescription = description
}

object GenTransferable {
  /**
   * Create a new data flavor for a given class (local to the JVM).
   * @param clazz The class to map on the DataFlavor
   */
  def makeFlavor[T](clazz: Class[T]): DataFlavor =
    new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=" + clazz.getCanonicalName,
      clazz.getSimpleName,
      this.getClass.getClassLoader) // <- Needed to allow usage of other class loaders
}
