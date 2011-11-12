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

import java.awt.datatransfer.Transferable
import javax.swing.ImageIcon
import java.awt.dnd._
import collection.mutable.WeakHashMap
import swing.{Button}
import java.awt.{Component => AwtComponent}

/**
 * Drag and drop source handler to be used with DragAndDropController.
 */
trait DragAndDropSource {

  def getComponent: AwtComponent

  def getTransferable: Option[Transferable]

  /**
   * Callback for the start of the drag operation.
   */
  def dragStarted(transferable: Transferable) {}

  /**
   * Callback for when the drag operation has ended.
   */
  def dragFinished(transferable: Transferable) {}
}

object DragAndDropSource {

  /**
   * Create a DragAndDropSource to wrap a button. On a dragStarted the button will be depressed without calling the actions
   * @param action Transferable that will be used during drag operation
   */
  def fromButton(button: Button, action: () => Transferable): DragAndDropSource = new DragAndDropSource {

    def getComponent: AwtComponent = button.peer

    def getTransferable: Option[Transferable] = Option(action())

    override def dragStarted(transferable: Transferable) {
      button.peer.getModel.setArmed(false)
      button.peer.getModel.setPressed(false)
      button.enabled = false
    }

    override def dragFinished(transferable: Transferable) {
      button.enabled = true
    }
  }
}

class DragAndDropController(icon: ImageIcon) extends DragGestureListener with DragSourceListener {

  //Setup
  private val ds: DragSource = new DragSource
  private val ghost = new DragAndDropGhost(icon.getImage)
  ds.addDragSourceMotionListener(ghost)
  private val sources = WeakHashMap.empty[java.awt.Component, DragAndDropSource]

  /**
   *
   */
  def enableDragToCopy(source: DragAndDropSource) {
    ds.createDefaultDragGestureRecognizer(source.getComponent, DnDConstants.ACTION_COPY, this)
    sources += (source.getComponent -> source)
  }

  def dragGestureRecognized(dge: DragGestureEvent) {
    if (dge.getComponent.isEnabled) {
      val dragComponent = sources.getOrElse(dge.getComponent, null)
      if (dragComponent != null) {
        val transferOption = dragComponent.getTransferable
        if (transferOption.isDefined) {
          dge.startDrag(null, transferOption.get, this)
          dragComponent.dragStarted(transferOption.get)
        }
      }
    }
  }

  def dragEnter(dsde: DragSourceDragEvent) {
    dsde.getDragSourceContext.setCursor(DragSource.DefaultCopyDrop)
  }

  def dragOver(dsde: DragSourceDragEvent) {
  }

  def dropActionChanged(dsde: DragSourceDragEvent) {
  }

  def dragExit(dse: DragSourceEvent) {
    dse.getDragSourceContext.setCursor(DragSource.DefaultCopyNoDrop)
  }

  def dragDropEnd(dsde: DragSourceDropEvent) {
    val dragComponent = sources.get(dsde.getDragSourceContext.getComponent)
    ghost.dragDropEnd(dsde)
    if (dragComponent.isDefined) dragComponent.get.dragFinished(dsde.getDragSourceContext.getTransferable)
  }
}

