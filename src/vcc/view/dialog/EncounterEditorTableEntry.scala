//$Id$
package vcc.view.dialog

import vcc.model.CombatantType

class EncounterEditorTableEntry(val ctype:CombatantType.Value) {
  var id:String=""
  var qty:Int=1
  var name:String="New "+ctype
  var hp:Int=if(ctype==CombatantType.Minion) 1 else 2
  var ac:Int=0
  var fortitude:Int=0
  var reflex:Int=0
  var will:Int=0
}

object EnconterEditorTableEntryProjection extends vcc.util.swing.TableModelRowProjection[EncounterEditorTableEntry] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("ID",classOf[String]),
    ("Qty",classOf[Integer]),
    ("Name",classOf[String]),
    ("HP",classOf[Integer]),
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
    case (4,entry,v)=> entry.ac=v.asInstanceOf[Int]
    case (5,entry,v)=> entry.fortitude=v.asInstanceOf[Int]
    case (6,entry,v)=> entry.reflex=v.asInstanceOf[Int]
    case (7,entry,v)=> entry.will=v.asInstanceOf[Int]

  }
  
  
  def apply(col:Int,entry:EncounterEditorTableEntry):java.lang.Object = {
    implicit val conv:Int=>java.lang.Object=int2Integer
    col match {
      case 0 => entry.id
      case 1 => entry.qty
      case 2 => entry.name
      case 3 => entry.hp
      case 4 => entry.ac
      case 5 => entry.fortitude
      case 6 => entry.reflex
      case 7 => entry.will
    }
  }
  
}