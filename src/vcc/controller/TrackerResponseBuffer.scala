//$Id$
package vcc.controller

/**
 * TrackerResponseBuffer is used to collect messages to be sent to Tracker observers.
 * This allows a single reply message or any number of messages to be stored. Tracker
 * will use this to send the messages to the observers.
 */
class TrackerResponseBuffer {
  
  private var outBound:List[Any]=Nil 
  
  private var _reply:Any=null
  
  /**
   * Set a single message that should be sent a a reply
   * @param msg The message
   * @throw Exception If the reply is set more then once
   */
  def reply(msg:Any) {
    if(_reply!=null) throw new Exception("Only can send one reply")
    else _reply=msg
  }
  
  /**
   * Get the response message if any (null otherwise)
   */
  def replyMessage = _reply
  
  /**
   * To emulate actor appearence, the buffer has a message sent ! method
   * @param msg Any message
   */
  def !(msg:Any) {
    outBound=msg::outBound
  }
  
  /**
   * Return the message that have been stored in the buffer, in the order
   * they were added to the buffer.
   */
  def messages:List[Any] = outBound.reverse

}

