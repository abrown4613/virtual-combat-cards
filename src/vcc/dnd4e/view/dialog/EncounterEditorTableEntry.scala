//$Id$
package vcc.dnd4e.view.dialog

import vcc.dnd4e.model.{CombatantType,CombatantTemplate,DefenseBlock}

object EncounterEditorTableEntry {
  def fromCombatantTemplate(ct:CombatantTemplate):EncounterEditorTableEntry = {
    var eete=new EncounterEditorTableEntry(ct.ctype)
    eete.name=ct.name
    eete.init=ct.init
    eete.hp=ct.hp
    eete.id=ct.id
    eete.ac=if(ct.defense!=null) ct.defense.ac else 0
    eete.fortitude=if(ct.defense!=null) ct.defense.fortitude else 0
    eete.reflex=if(ct.defense!=null) ct.defense.reflex else 0
    eete.will=if(ct.defense!=null) ct.defense.will else 0
    eete
  }
}
class EncounterEditorTableEntry(val ctype:CombatantType.Value) {
  var id:String=""
  var qty:Int=1
  var name:String="New "+ctype
  var hp:Int=if(ctype==CombatantType.Minion) 1 else 2
  var init:Int=0
  var ac:Int=0
  var fortitude:Int=0
  var reflex:Int=0
  var will:Int=0
  
  def isSame(that:EncounterEditorTableEntry):Boolean = 
    ((hp==that.hp) && (this.name==that.name) && (this.init==that.init) && 
     (id==null || id=="") && (that.id==null || that.id=="") &&
     (ac==that.ac) && (fortitude == that.fortitude) && (reflex==that.reflex) && (will==that.will))
  
  def singleCopy():EncounterEditorTableEntry = {
    var nc= new EncounterEditorTableEntry(ctype)
    nc.id=id
    nc.name=name
    nc.hp=hp
    nc.init=init
    nc.ac=ac
    nc.reflex=reflex
    nc.will=will
    nc.fortitude=fortitude
    nc
  }
  
  /**
   * Creates a single CombatantTemplate based on the entry, if qty is greater than one
   * only a single combatant will be returned.
   */
  def toSingleCombatantTemplate():CombatantTemplate = {
    var ct=new CombatantTemplate(name,hp,init,ctype)
    if(id!=null && id!="") ct.id=id
    var db=DefenseBlock(ac,fortitude,reflex,will)
    if(db!=DefenseBlock(0,0,0,0))
      ct.defense=db
    ct
  }
}

