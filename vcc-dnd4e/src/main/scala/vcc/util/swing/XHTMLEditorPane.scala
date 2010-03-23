//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing

import scala.swing._
import scala.swing.event._

class XHTMLEditorPane(starttext:String,otherActions:Action *) extends MigPanel("fill","[200][200]","[][300]"){
  
  private val xhtmlPane = new XHTMLPane()
  private val editPane = new TextArea(starttext)
  
  // Construction
  xhtmlPane.setDocumentFromText(starttext)
  add(new Button(Action("Preview"){xhtmlPane.setDocumentFromText(editPane.text)}),
      if(otherActions.isEmpty) "span 2,wrap" else "span 2, split " + otherActions.length + 1)
  otherActions.foreach {x => add(new Button(x),if(x == otherActions.last) "wrap" else "")}
  add(new ScrollPane(editPane),"growx, growy")
  add(xhtmlPane,"growx, growy")
  
  def text:String = editPane.text
  def text_=(txt:String) { editPane.text = txt }
  
  def sync() {
    xhtmlPane.setDocumentFromText(editPane.text)
  }
}
