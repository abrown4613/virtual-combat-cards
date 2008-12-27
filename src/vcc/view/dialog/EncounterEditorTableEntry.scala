//$Id$
package vcc.view.dialog

import vcc.model.CombatantType

object EncounterEditorTableEntry {
  def fromCombatantTemplate(ct:vcc.model.CombatantTemplate):EncounterEditorTableEntry = {
    var eete=new EncounterEditorTableEntry(ct.ctype)
    eete.name=ct.name
    eete.init=ct.init
    eete.hp=ct.hp
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
}

object EnconterEditorTableEntryProjection extends vcc.util.swing.TableModelRowProjection[EncounterEditorTableEntry] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("ID",classOf[String]),
    ("Qty",classOf[Integer]),
    ("Name",classOf[String]),
    ("HP",classOf[Integer]),
    ("Init",classOf[Integer]),
    ("AC",classOf[Integer]),
    ("Fort",classOf[Integer]),
    ("Refl",classOf[Integer]),
    ("Will",classOf[Integer])
  )

  val setter:PartialFunction[(Int,EncounterEditorTableEntry,Any),Unit]= {
    case (0,entry,v) if(entry.qty==1)=> 
      entry.id=v.asInstanceOf[String].toUpperCase
    case (1,entry,v) if(entry.id=="")=> 
      entry.qty=v.asInstanceOf[Int]
      if(entry.qty<1) entry.qty=1
    case (2,entry,v) => 
      entry.name=v.asInstanceOf[String]
    case (3,entry,v) if(entry.ctype!=CombatantType.Minion)=> 
      entry.hp=v.asInstanceOf[Int]
      if(entry.hp<2) entry.hp=2
    case (4,entry,v)=> entry.init=v.asInstanceOf[Int]
    case (5,entry,v)=> entry.ac=v.asInstanceOf[Int]
    case (6,entry,v)=> entry.fortitude=v.asInstanceOf[Int]
    case (7,entry,v)=> entry.reflex=v.asInstanceOf[Int]
    case (8,entry,v)=> entry.will=v.asInstanceOf[Int]
  }
  
  
  def apply(col:Int,entry:EncounterEditorTableEntry):java.lang.Object = {
    implicit val conv:Int=>java.lang.Object=int2Integer
    col match {
      case 0 => entry.id
      case 1 => entry.qty
      case 2 => entry.name
      case 3 => entry.hp
      case 4 => entry.init
      case 5 => entry.ac
      case 6 => entry.fortitude
      case 7 => entry.reflex
      case 8 => entry.will
    }
  }
  
}