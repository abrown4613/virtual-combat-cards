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
package test.dnd4e
import junit.framework.TestCase
import vcc.dnd4e.model._
import vcc.controller.transaction._
import vcc.dnd4e.controller._
import vcc.controller.TransactionalProcessor

class ContextLoaderTest extends TestCase {
  
  var context=new TrackerContext()
  
  def testLoadCombatant {
    //TODO: This is not the real way to do it, much better, but needs improvement
    val handler=new TransactionalProcessor(context) with TrackerContextHandler
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    //FIXME handler.dispatch(trans1,request.AddCombatant(new CombatantTemplate("Figher",40,5,CombatantType.Character){id="A"}))
    //FIXME handler.dispatch(trans1,request.AddCombatant(new CombatantTemplate("Monster",80,5,CombatantType.Character)))
    
    trans1.commit(trans1pub)
    assert(trans1pub.set.contains(vcc.dnd4e.view.actor.SetSequence(List('A,Symbol("1")))))
  }

}

class SetChangePublisher extends TransactionChangePublisher {
  val set=scala.collection.mutable.Set.empty[ChangeNotification]
  def publishChange(seq:Seq[ChangeNotification]) {
    for(c<-seq) set+=c
  }
}

