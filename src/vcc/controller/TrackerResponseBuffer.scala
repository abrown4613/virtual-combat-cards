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

