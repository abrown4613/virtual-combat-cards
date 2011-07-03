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
package vcc.infra.util

import java.io.InputStream
import xml.{Node, Text, XML}

object RssItem {
  val lineBreak = Text("\n")
  val tabStop = Text("\t")

  def fromXML(item: Node) = RssItem(
    (item \ "guid").text,
    (item \ "title").text,
    (item \ "link").text,
    (item \ "description").text)
}

case class RssItem(guid: String, title: String, link: String, description: String) {

  import RssItem._

  def toXML = {
    val parts = Seq(
      <title>{title}</title>,
      <link>{link}</link>,
      <description>{description}</description>,
      <guid>{guid}</guid>)

    <item>{lineBreak}{parts.flatMap(x => Seq(tabStop, x, lineBreak))}</item>
  }
}

object RssLoader {
  def load(is: InputStream): Seq[RssItem] = {
    val xml = XML.load(is)
    (xml \\ "item").map {
      item => RssItem.fromXML(item)
    }.toSeq
  }

  def main(args: Array[String]) {

    //val is = new java.io.FileInputStream(new java.io.File("c:/temp/rss.xml"))
    val is = new java.net.URL("http://www.exnebula.org/taxonomy/term/17/0/feed").openStream()
    val list = RssLoader.load(is)
    list.foreach(x => printf("Title: %s\nLink: %s\nDescription:\n%s\nGuid:%s\n", x.title, x.link, x.description, x.guid))
  }
}
