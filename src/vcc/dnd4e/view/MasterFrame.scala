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
package vcc.dnd4e.view

import scala.actors.Actor
import scala.swing._

import vcc.model.Registry

class MasterFrame extends MainFrame{

  val tracker = Registry.get[Actor]("tracker").get

  var uia=new vcc.dnd4e.view.actor.UserInterface(tracker)
  
  val commandPanel= new vcc.dnd4e.view.CombatantActionPanel(uia,tracker)
  val seqTable = new vcc.dnd4e.view.SequenceTable(uia,tracker)
  seqTable.minimumSize = new java.awt.Dimension(300,200)
  val card=new vcc.dnd4e.view.CombatantCard(tracker)
  val statusBar=new StatusBar(uia)
  
  // Register panel with UIA
  uia.addSequenceListener(seqTable)
  uia.addSequenceListener(commandPanel)
  uia.addContextListener(commandPanel)
  uia.addContextListener(commandPanel.effectEditorPanel)
  uia.addContextListener(seqTable)
  uia.addContextListener(card)
  uia.setStatusBar(statusBar)
  uia.start
  tracker ! vcc.controller.actions.AddObserver(uia)

  title = "Virtual Combat Cards"
    
  preferredSize= {
	val toolkit = java.awt.Toolkit.getDefaultToolkit();
	val dimension = toolkit.getScreenSize();
	if(dimension!=null)
		new java.awt.Dimension(if(dimension.getWidth()>=1150) 1150 else 800, if(dimension.getHeight()>=700) 690 else 600)
	else
		new java.awt.Dimension(800,600) 
  }
  val split = new SplitPane(Orientation.Vertical,seqTable,card)
  split.peer.setDividerSize(4)
  split.peer.setDividerLocation(440)
    
  contents= new BorderPanel {
	  add(commandPanel,BorderPanel.Position.West)
      add(split,BorderPanel.Position.Center)
      add(new MainMenu(uia),BorderPanel.Position.North)
      add(statusBar,BorderPanel.Position.South)
      iconImage=IconLibrary.MetalD20.getImage()
  }
}
