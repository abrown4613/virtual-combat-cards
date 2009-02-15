//$Id$
package test.helper

import vcc.controller._
import vcc.controller.transaction._
/**
 * Tracker mockup has several utility methods to help test action handlers and publishers. It is on
 * and actor so it works syncronly.
 */
class TrackerMockup[C](controller:TrackerController[C]) extends TransactionChangePublisher {

  private var pbuf:TrackerResponseBuffer=null
  
  def publishChange(changes:Seq[ChangeNotification]) {
    pbuf=new TrackerResponseBuffer()
    controller.publish(changes,pbuf)
  }
  
  /**
   * Dispatch and action and return the transaction
   */
  def dispatch(msg:actions.TransactionalAction):Transaction = {
    val trans=new Transaction()
    controller.dispatch(trans,msg)
    trans
  }
  
  /**
   * Commit the transaction
   */
  def commitTransaction(trans:Transaction) = {
    trans.commit(this)
  }
  
  /**
   * Return the last TrackerResponseBuffer's message that would be sent
   */
  def lastChangeMessages = pbuf.messages
}
