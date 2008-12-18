package vcc.view.dialog

import vcc.util.swing.TableModelRowProjection
import vcc.model.DiceBag

//NOTE: These classes should be internal to the Dialog 

class InitiativeDialogEntry(val id:Symbol, val name:String, var init:Int, var roll:Int, var reserve:Boolean) extends Ordered[InitiativeDialogEntry] {
  def compare(that:InitiativeDialogEntry):Int= {
    var diff=(roll+init)-(that.roll+that.init)
    if(diff==0) {
      diff=init-that.init
      if(diff==0) return if(DiceBag.flipCoin) -1 else 1
    }
    -diff
  }
  override def toString():String="IDEntry("+id+","+name+","+init+","+roll+","+reserve+")"
}

object InitiativeDialogEntryProjection extends TableModelRowProjection[InitiativeDialogEntry] {
  val columns=List[(String,java.lang.Class[_])](
    ("ID",classOf[String]),
    ("Name",classOf[String]),
    ("Init",classOf[Integer]),
    ("Roll",classOf[Integer]),
    ("Reserve",classOf[Boolean]))
  def apply(col:Int,entry:InitiativeDialogEntry):java.lang.Object= {
    col match {
      case 0 => entry.id.name
      case 1 => entry.name
      case 2 => int2Integer(entry.init)
      case 3 => int2Integer(entry.roll)
      case 4 => boolean2Boolean(entry.reserve)
    }
  }
  
  val setter:PartialFunction[(Int,InitiativeDialogEntry,Any),Unit]= {
    case (2,entry,v) => entry.init=v.asInstanceOf[Int]
    case (3,entry,v) => entry.roll=v.asInstanceOf[Int]
    case (4,entry,v) => entry.reserve=v.asInstanceOf[Boolean]
  }
}
