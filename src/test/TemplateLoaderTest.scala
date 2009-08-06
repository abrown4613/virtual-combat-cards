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
package test

import junit.framework.TestCase

import vcc.dnd4e.model._

class TemplateLoaderTest extends TestCase {
  
  /*FIXME
  def testLoadMinion() {
    var tmpl=CombatantTemplate.fromXML(<minion name="Goblin Cutter" init="5"></minion>)
    assert(tmpl.name=="Goblin Cutter")
    assert(tmpl.ctype==CombatantType.Minion)
    assert(tmpl.init==5)
    assert(tmpl.hp==1)
    assert(tmpl.id==null)
    assert(tmpl.defense==null)
  }
  
  def testLoadMonster() {
    var tmpl=CombatantTemplate.fromXML(<monster name="Goblin Blackblade" hp="25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </monster>)
    assert(tmpl.name=="Goblin Blackblade")
    assert(tmpl.ctype==CombatantType.Monster)
    assert(tmpl.init==7)
    assert(tmpl.hp==25)
    assert(tmpl.id==null)
    
  }
  
  def testLoadCharacter() {
    
    //var node:scala.xml.Node=
    var tmpl=CombatantTemplate.fromXML(<character id="k" name="Kantrex" hp="44" init="5"><defense ac="23" will="18" reflex="17" fortitude="18" /> </character>)
    assert(tmpl.name=="Kantrex")
    assert(tmpl.ctype==CombatantType.Character)
    assert(tmpl.init==5)
    assert(tmpl.hp==44)
    assert(tmpl.id=="K")
    
    assert(tmpl.defense!=null)
    assert(tmpl.defense.ac==23)
    assert(tmpl.defense.reflex==17)
    assert(tmpl.defense.will==18)
    assert(tmpl.defense.fortitude==18)
  }
  
  def testBadXML() {
    try {
      var tmpl=CombatantTemplate.fromXML(<combatant name="Goblin Blackblade" hp="25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </combatant>)
      assert(false,"Failed raise exception")
    } catch {
      case e:Exception => true
    }
    try {
      var tmpl=CombatantTemplate.fromXML(<monster name="Goblin Blackblade" hp="a25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </monster>)
      println(tmpl.hp)
      assert(false,"Failed raise exception")
    } catch {
      case e:Exception => true
    }
    
    // Name is not defines, this is a problem should throw exception
    try {
      var tmpl=CombatantTemplate.fromXML(<monster hp="25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </monster>)
      assert(false,"Failed raise exception")
    } catch {
      case e:Exception => true
    }
  }
  
  
  def testPartyLoader() {
    var l=PartyLoader.loadFromXML(
      <party>
        <minion name="Goblin Cutter" init="5"></minion>
        <monster name="Goblin Blackblade" hp="25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </monster>
        <minion name="Goblin Cutter" init="5"></minion>
        <monster name="Goblin Blackblade" hp="25" init="7"><defense ac="16" fortitude="12" reflex="14" will="11" /> </monster>
        <minion name="Goblin Cutter" init="5"></minion>
        <minion name="Goblin Cutter" init="5"></minion>
        <character id="k" name="Kantrex" hp="44" init="5"><defense ac="23" will="18" reflex="17" fortitude="18" /> </character>
        <character id="A" name="Azathoth" hp="45" init="4"><defense ac="17" will="20" reflex="19" fortitude="17" /> </character>
      </party>)
    assert(l.size==8)
    assert((l filter(x=>x.id != null)).size==2)
    //FIXME assert((l filter(x=>x.name != "Goblin Cutter")).size==4)
  }

  def testDefenseBlock {
    var db=DefenseBlock(20,19,18,17)
    assert(db.toXML == 
      <defense ac="20" fortitude="19" reflex="18" will="17" />)
    
    db=DefenseBlock.fromXML(
      <defense ac="22" />
    )
    assert(db.ac==22)
    assert(db.fortitude==0)
    assert(db.will==0)
    assert(db.reflex==0)
  }
  */
  def testSaving() {
    /*FIXME
     
    var chr=new CombatantTemplate("test1",50,3,CombatantType.Character)
    chr.defense=DefenseBlock(20,19,18,17)
    chr.id="A"
    var xml=chr.toXML
    var chrl=CombatantTemplate.fromXML(xml)
    assert(chrl isSame chr)

    chr=new CombatantTemplate("test2",55,4,CombatantType.Monster)
    chr.defense=DefenseBlock(20,19,18,17)
    xml=chr.toXML
    chrl=CombatantTemplate.fromXML(xml)
    assert(chrl isSame chr)

    chr=new CombatantTemplate("test2",1,4,CombatantType.Minion)
    xml=chr.toXML
    println(xml)
    chrl=CombatantTemplate.fromXML(xml)
    assert(chrl isSame chr)
     */
  
  }
}
