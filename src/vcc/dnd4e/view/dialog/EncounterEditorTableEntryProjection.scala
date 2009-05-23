//$Id$
package vcc.dnd4e.view.dialog

import vcc.dnd4e.model.CombatantType

object EncounterEditorTableEntryProjection extends vcc.util.swing.TableModelRowProjection[EncounterEditorTableEntry] {
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
    case (1,entry,v) if(entry.id=="" || entry.id == null )=> 
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