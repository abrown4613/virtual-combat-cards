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

import org.specs.SpecificationWithJUnit
import java.io.ByteArrayInputStream

class RssLoaderTest extends SpecificationWithJUnit {
  val sampleItem = RssItem("guid", "title", "link", "description")
  "RssItem" should {
    "save and load itself" in {
      sampleItem must_== RssItem.fromXML(sampleItem.toXML)
    }

    "load from XML" in {
      val x = <item>
        <title>title</title>
        <link>link</link>
        <description>description</description>
        <guid>guid</guid>
      </item>
      RssItem.fromXML(x) must_== sampleItem
    }
  }
  "RssLoader" should {
    "load a complete feed" in {
      val feed = <rss xml:base="http://www.exnebula.org" version="2.0" xmlns:dc="http://purl.org/dc/elements/1.1/">
        <channel>
          <title>Ex Nebula - Virtual Combat Card News</title>
          <link>http://www.exnebula.org/taxonomy/term/17/0</link>
          <description>New for the Virtual Combat Card project.</description>
          <language>en</language>
          <item>
            <title>title</title>
            <link>link</link>
            <description>description</description>
            <comments>http://www.exnebula.org/node/237#comments</comments>
            <category domain="http://www.exnebula.org/taxonomy/term/4">RPG</category>
            <category domain="http://www.exnebula.org/taxonomy/term/17">Virtual Combat Card News</category>
            <pubDate>Fri, 24 Jun 2011 03:25:34 +0000</pubDate>
            <dc:creator>Nebulorum</dc:creator>
            <guid isPermaLink="false">guid</guid>
          </item>
        </channel>
      </rss>

      val l = RssLoader.load(new ByteArrayInputStream(feed.toString().getBytes))

      l must_== List(sampleItem)
    }
  }
}