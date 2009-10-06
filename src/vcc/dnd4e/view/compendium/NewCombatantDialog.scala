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
  
package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.dnd4e.domain.compendium.{CombatantEntity,Compendium}

class NewCombatantDialog(owner: Frame) extends ModalDialog[CombatantEntity](owner,"New Compendium Entry") {

  val helpText = (<html><body>
    D&amp;D Insider ID is the number that appear in the URL from the monster on Wizards site.
    If you are going to insert an official monster we recommend you lookup the ID or use the D&amp;D Insider Capture Firefox plug-in.
    Using a standard ID will allow sharing of files and VCC specific information in the future.
  </body></html>).text

  private val idField = new TextField()                                                                                                   
  private val idHelp = Action("?") {
    Dialog.showMessage(idField,helpText)
  } 
  
  private val characterRadioButton = new RadioButton("Character")
  private val customMonsterRadioButton = new RadioButton("Monster")
  private val officialMonsterRadioButton = new RadioButton("D&D Official Monster")
  
  private val buttonGroup = new ButtonGroup(characterRadioButton,customMonsterRadioButton,officialMonsterRadioButton)
  buttonGroup.select(customMonsterRadioButton)
  
  idField.enabled = false
 
  contents = new MigPanel("fillx") {
    add(customMonsterRadioButton,"wrap")
    add(characterRadioButton,"wrap")
    add(officialMonsterRadioButton,"wrap")
    add(new Label("D&D Insider ID:"),"split 5, gapleft 25")
    add(idField,"growx")
    add(new Button(idHelp),"wrap")
    add(new Button(okAction),"split 3")
    add(new Button(cancelAction))
  }
  minimumSize = new java.awt.Dimension(250,200)
  
  private def toggleOkAction() {
    okAction.enabled = if(idField.enabled)
      try{ this.idField.text.toInt; true } catch { case _ => false }
    else 
      true
  }
    
   
  listenTo(characterRadioButton,customMonsterRadioButton,officialMonsterRadioButton,idField)
  reactions += {
    case ButtonClicked(btn) =>
      if(btn == officialMonsterRadioButton) idField.requestFocus()
      idField.enabled = officialMonsterRadioButton.selected
      toggleOkAction()
    case ValueChanged(this.idField) => 
      toggleOkAction()
  }
  
  def processOK() {
    dialogResult = buttonGroup.selected match { 
      /* FIXME 
      case Some(this.characterRadioButton) => Some(Compendium.createNewCharacter)
      case Some(this.customMonsterRadioButton) => Some(Compendium.createNewMonster)
      case Some(this.officialMonsterRadioButton) => Some(Compendium.createNewOfficialMonster(idField.text.toInt))
       */
      case _ => None
    }
  }
}
