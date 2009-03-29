//$Id$
package vcc.dnd4e.view.helper

import vcc.dnd4e.view.dialog.InitiativeDialogEntry
import vcc.util.DiceBag
import scala.util.Sorting.stableSort

object InitiativeRoller {
  
  case class GroupInitEntry(roll:Int,initBonus:Int,ids:Set[Symbol]) extends Ordered[GroupInitEntry] {
    def compare(that:GroupInitEntry):Int = {
      var diff=(this.roll+this.initBonus)-(that.roll + that.initBonus)
      if(diff==0) {
        diff=this.initBonus - that.initBonus
        if(diff==0) return if(DiceBag.flipCoin) -1 else 1
      }
      -diff
    }
    
    def rollIfZeroed():GroupInitEntry = {
      if(roll<=0) GroupInitEntry(DiceBag.D(20),initBonus,ids)
      else this
    }
  }
  
  /**
   * Create Initiative Groups, this will collect essencial data. If initiative
   * values are present the smallest one of the group is used
   * @param joinSimilar Join objects that have the same name
   * @param ie InitiativeDialogEntries that contain the values you need
   * @return a list of groups to be sorted (roll, initiative bonus, Set[Symbol])
   */
  def createInitiativeGroups(joinSimilar:Boolean, ide:List[InitiativeDialogEntry]):List[GroupInitEntry] = {
    val ie=ide.filter(e=> ! e.reserve)
    if(joinSimilar) {
      var map=scala.collection.mutable.Map.empty[(String,Int),(Int,Set[Symbol])]
      for(e<-ie) {
        val p=(e.name,e.init)
        if(map.contains(p)) {
          val old=map(p)
          map += p -> (Math.max(e.roll,old._1), old._2 + e.id )
        }else {
          map += p->(e.roll,Set(e.id)) 
        }
      }
      map.map{ case ((n,init),(roll,ids)) => GroupInitEntry(roll,init,ids)}.toList 
    } else {
      ie.map(e=> GroupInitEntry(e.roll,e.init,Set(e.id) ))
    }
  }
  
  /**
   * Sort initiative for each grou and return an ordered sequence of symbols
   * @param groups Groups as created by createInitiativeGroups
   * @return Sequence of symbols
   */
  def sortGroups(groups:List[GroupInitEntry]):List[Symbol] = {
    
    val ord=stableSort(groups)
    //val idlist=ord.m
    val ordIds=ord.map(x=> stableSort(x.ids.toSeq,(a:Symbol,b:Symbol)=>{ a.name <= b.name }))
    ordIds.flatMap(x=>x.toList).toList
  }
  
  /**
   * Roll initiative for the group. 
   * @param joinSimilar Join objects that have the same name
   * @param ie InitiativeDialogEntries that contain the values you need
   * @return Sequence of symbols ordered by initiative
   */
  def rollInitiative(joinSimilar:Boolean, ie:List[InitiativeDialogEntry]):List[Symbol] = {
    var l=createInitiativeGroups(joinSimilar,ie)
    l=l.map(e=>e.rollIfZeroed)
    sortGroups(l).toList
  }
}
