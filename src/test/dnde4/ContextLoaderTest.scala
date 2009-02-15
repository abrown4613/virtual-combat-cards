//$Id$
package test.dnde4
import junit.framework.TestCase
import vcc.dnd4e.model._
import vcc.controller.transaction._
import vcc.dnd4e.controller._

class SetChangePublisher extends TransactionChangePublisher {
  val set=scala.collection.mutable.Set.empty[ChangeNotification]
  def publishChange(seq:Seq[ChangeNotification]) {
    for(c<-seq) set+=c
  }
}

class ContextLoaderTest extends TestCase {
  
  var context=new TrackerContext()
  
  def testLoadCombatant {
    //TODO: This is not the real way to do it, much better, but needs improvement
    val handler=new TrackerContextHandler(context)
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    handler((trans1,actions.AddCombatant(new CombatantTemplate("Figher",40,5,CombatantType.Character){id="A"})))
    handler((trans1,actions.AddCombatant(new CombatantTemplate("Monster",80,5,CombatantType.Character))))
    
    trans1.commit(trans1pub)
    println(trans1pub.set)
    assert(trans1pub.set.contains(vcc.dnd4e.view.actor.SetSequence(List('A,Symbol("1")))))
  }

}
