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
package vcc.dnd4e.view


import vcc.infra.docking.{DockID, ScalaDockableComponent}
import vcc.util.swing.{XHTMLPane, MigPanel}
import javax.swing.{SwingWorker, JComponent}
import swing.Frame
import vcc.infra.util.RssLoader
import org.xhtmlrenderer.swing.{BasicPanel, LinkListener}
import java.awt.{Desktop, Dimension}
import java.net.URL

class NewsPanel extends MigPanel("fill") with ScalaDockableComponent {
  def dockFocusComponent: JComponent = null

  def dockID: DockID = DockID("project-news")

  def dockTitle: String = "Project News"

  private val xhtml = new XHTMLPane() //(new NaiveUserAgent())
  xhtml.removeLinkListener()

  xhtml.addMouseTrackingListener(new LinkListener {
    override def linkClicked(panel: BasicPanel, uri: String) {
      System.err.println("You clicked on: " + uri)
      if (uri != null) {
        val finalUri = if (uri.startsWith("http://")) uri else "http://www.exnebula.org" + uri
        System.err.println("Sending your browser to:" + finalUri)
        Desktop.getDesktop.browse(new URL(finalUri).toURI)
      }
    }
  })

  add(xhtml, "growx, growy")
  xhtml.setDocumentFromText("<html><body>Getting news</body></html>")
  preferredSize = new Dimension(300, 300)
  minimumSize = preferredSize

  def updateIfOld(age: Int, docker: CustomDockingAdapter, frame: Frame) {
    if (true) {
      //TODO Check for update
      //Fido is our new fetcher (long live FidoNet)
      val fido = new SwingWorker[Option[String], Void] {
        //TODO Hard logic in fido

        def doInBackground(): Option[String] = {
          val is = new java.net.URL("http://www.exnebula.org/taxonomy/term/17/0/feed").openStream()
          val list = RssLoader.load(is)
          val news = list.map(x =>
            String.format("""<h1><a href="%s">%s</a></h1>%s""", x.link, x.title, x.description)).mkString("")
          xhtml.setDocumentFromText("<html>" +
            "<head><link href=\"dndi.css\" type=\"text/css\" rel=\"stylesheet\"></link></head>" +
            "<body>" + news + "</body></html>")
          None
        }

        override def done() {
          docker.restoreFocus(DockID("project-news"))
        }
      }
      fido.execute()
    }
  }
}