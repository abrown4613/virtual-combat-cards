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
import javax.swing.{SwingWorker, JComponent}
import org.xhtmlrenderer.swing.{BasicPanel, LinkListener}
import java.net.URL
import vcc.infra.util.{RssItem, RemoteFile, RssLoader}
import java.io.{InputStream, File}
import java.awt.{Desktop, Dimension}
import vcc.util.UpdateManager
import swing._
import vcc.util.swing.{SwingHelper, XHTMLPane, MigPanel}
import vcc.dnd4e.{BootStrap, Configuration}
import org.slf4j.LoggerFactory

/**
 * Visual area for displaying news and informing of version updates.
 */
class NewsPanel extends MigPanel("fill", "", "[grow 0]5[grow 100]") with ScalaDockableComponent {
  def dockFocusComponent: JComponent = null

  def dockID: DockID = DockID("project-news")

  def dockTitle: String = "Project News"

  private val xhtml = new XHTMLPane()
  xhtml.removeLinkListener()

  private val remoteFile = new RemoteFile(
    new File(Configuration.baseDirectory.value, "feed.rss"),
    new URL("http://www.exnebula.org/taxonomy/term/17/0/feed"))

  xhtml.addMouseTrackingListener(new LinkListener {
    override def linkClicked(panel: BasicPanel, uri: String) {
      if (uri != null) {
        val finalUri = if (uri.startsWith("http://")) uri else "http://www.exnebula.org" + uri
        Desktop.getDesktop.browse(new URL(finalUri).toURI)
      }
    }
  })

  private val fidoButton = new Button(Action("Check for news") {
    doFetch(null, this, 0)
  })

  private val versionStatus = new Label("Your version is up to date")
  private val upgradeButton = new Button(Action("Upgrade...") {
    SwingHelper.invokeInOtherThread {
      // We provide one hour cache duration to avoid double fetch.
      UpdateManager.runUpgradeProcess(Configuration.getVersionReleaseURL, BootStrap.version, IconLibrary.MetalD20.getImage, 3600 * 1000)
    }
  })
  upgradeButton.enabled = false
  upgradeButton.visible = false

  add(fidoButton, "split 3")
  add(versionStatus, "gap 30")
  add(upgradeButton, "wrap")
  add(xhtml, "growx, growy")
  xhtml.setDocumentFromText("<html><body>Getting news. Please wait or click the <strong>Check for news</strong> button.</body></html>")
  preferredSize = new Dimension(300, 300)
  minimumSize = preferredSize


  private def readFeed(is: InputStream): Seq[RssItem] = {
    try {
      if (is != null) {
        RssLoader.load(is)
      } else {
        Seq()
      }
    } catch {
      case _ => Seq()
    }
  }

  def formatFeed(items: Seq[RssItem]): String = {
    val news = items.map(x =>
      String.format("""<h1 class="title"><a href="%s">%s</a></h1>%s""", x.link, x.title, x.description)).mkString("")
    ("<html>" +
      "<head><link href=\"feed.css\" type=\"text/css\" rel=\"stylesheet\"></link></head>" +
      "<body>" + news + "</body></html>")

  }


  private def updateContent(items: Seq[RssItem], mustUpdate: Boolean) {
    xhtml.setDocumentFromText(formatFeed(items))
    if (mustUpdate) {
      upgradeButton.enabled = true
      upgradeButton.visible = true
      this.versionStatus.text = "Your VCC needs an upgrade"
    }
  }

  def doFetch(docker: CustomDockingAdapter, panel: NewsPanel, age: Long) {
    val fido = new SwingWorker[Option[(Seq[RssItem], Boolean, Boolean)], Void] {

      def doInBackground(): Option[(Seq[RssItem], Boolean, Boolean)] = {
        try {
          val oldList = readFeed(remoteFile.getLocalCopy)
          val list = readFeed(remoteFile.fetchIfOlder(age))

          val needsUpgrade = UpdateManager.checkForUpgrade(Configuration.getVersionReleaseURL, BootStrap.version, Configuration.getCheckAfterAge)

          val hasNewNews = list.map(_.guid) != oldList.map(_.guid)
          Some((list, hasNewNews, needsUpgrade))
        }
        catch {
          case e =>
            LoggerFactory.getLogger("infra").error("Failed to fetch update.", e)
            None
        }
      }

      override def done() {
        if (this.get().isDefined) {
          val (news, isNewNews, needUpgrade) = this.get().get
          if (panel != null) panel.updateContent(news, needUpgrade)
          if (isNewNews || needUpgrade) {
            if (docker != null) docker.restoreFocus(DockID("project-news"))
          }
        }
      }
    }
    fido.execute()
  }

  def updateIfOld(age: Long, docker: CustomDockingAdapter) {
    if (!remoteFile.isOlderThan(0)) {
      //Show old feed
      xhtml.setDocumentFromText(formatFeed(readFeed(remoteFile.getLocalCopy)))
    } else {
      //Fido is our new fetcher (long live FidoNet)
      doFetch(docker, this, age)
    }
  }

}