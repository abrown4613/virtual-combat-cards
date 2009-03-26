//$Id$
package vcc.dnd4e.view.helper

import vcc.dnd4e.view.dialog.InitiativeDialogEntry

object InitiativeRoller {

  type E=(Int,Int,Set[Symbol])
  
  /**
   * Create Initiative Groups, this will collect essencial data. If initiative
   * values are present the smallest one of the group is used
   * @param joinSimilar Join objects that have the same name
   * @param ie InitiativeDialogEntries that contain the values you need
   * @return a list of groups to be sorted
   */
  def createInitiativeGroups(joinSimilar:Boolean, ie:List[InitiativeDialogEntry]):List[E] = {
    if(joinSimilar) {
      var map=scala.collection.mutable.Map.empty[(String,Int),(Int,Set[Symbol])]
      for(e<-ie) {
        val p=(e.name,e.init)
        if(map.contains(p)) {
          val old=map(p)
          map += p -> (Math.min(e.roll,old._1), old._2 + e.id )
        }else {
          map += p->(e.roll,Set(e.id)) 
        }
      }
      
      map.map{ case ((n,init),(roll,ids)) => (roll,init,ids)}.toList 
      
    } else {
      ie.map(e=> (e.roll,e.init,Set(e.id) ))
    }
  }
}
