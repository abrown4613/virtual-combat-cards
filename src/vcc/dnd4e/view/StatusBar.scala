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
//$Id$
package vcc.dnd4e.view

import vcc.util.swing.MigPanel
import vcc.dnd4e.BootStrap
import scala.swing._

class StatusBar() extends MigPanel("ins 1,fillx") {
  private val versionLabel= new Label("Version: "+BootStrap.version.versionString)
  private val tipLabel = new Label("Welcome to Virtual Combat Cards") 
  tipLabel.xAlignment= Alignment.Left

  border=javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED)
 
  private val sep=new Component { override lazy val peer= new javax.swing.JSeparator(javax.swing.SwingConstants.VERTICAL)}
  add(versionLabel, "align left")
  add(sep, "growy")
  add(tipLabel,"growx,align left")
 
  /**
   * Set the text in the tip area
   * @param str Text to be set
   */
  def setTipText(str:String) {
    tipLabel.text=str
  }
}
