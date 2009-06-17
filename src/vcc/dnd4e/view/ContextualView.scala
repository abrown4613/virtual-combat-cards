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
package vcc.dnd4e.view

class NotInContextException extends Exception("Not in Context")

trait ContextualView[T] {
  
  protected var _context:Option[T]=None
  
  def context_=(v:Option[T])= {
    //Warn of changing context prior to changing
    changeContext(v); 
    _context=v; 
  }
  
  def context:T=_context match { case Some(v)=>v; case None => throw new NotInContextException()}
  
  def isInContext:Boolean = _context.isDefined
  
  def changeContext(context:Option[T]) 
}
