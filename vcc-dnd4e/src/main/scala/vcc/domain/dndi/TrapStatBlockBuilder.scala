/**
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
package vcc.domain.dndi

import xml.Node

//TODO Delete
@deprecated("move to XTEMPLATE")
object TrapStatBlockBuilder extends StatBlockBuilder {
  import StatBlockBuilder._

  val block = new Group("Sections",
    IfDefined("header",XHTMLElement("SPAN","trapblocktitle", true, TextFormat("header",false))),
    InlineStyledText("text", true))

  def generate(ds: StatBlockDataSource): Node = {
    (<html>
       <head>
           <link rel="stylesheet" type="text/css" href="dndi.css"/>
       </head>
       <body>
         <div id="detail">
           <h1 class="trap">
             {ds.extract("NAME").get}<span class="type">
             {ds.extract("TYPE").get + " " + ds.extract("ROLE").get}
           </span>
           </h1>
           {block.render(ds)}
         </div>
       </body>
     </html>)

  }
}