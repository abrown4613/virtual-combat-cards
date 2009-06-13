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
package vcc.model

/**
 * Generates numeric IDs for Combatants with out ID. It works as a set of 
 * chips, so you can take them out of the pool, and back into it.
 */
class IDGenerator(start:Int, end:Int) {

  private var ids=List((start,end))
  
  private var leased:List[Symbol]=Nil
  
  /**
   * Remove the first ID from the pool, and returns the updated ID 
   */
  private def takeFirst():(Symbol,List[(Int,Int)]) = ids match {
    case (x,y)::rest if(x==y)=>(Symbol(x.toString),rest)
    case (x,y)::rest => (Symbol(x.toString),(x+1,y)::rest)
    case Nil => throw new Exception("Empty set")
  }
  
  /**
   * Put an ID back into the range
   */
  private def addToList(n:Int,ids:List[(Int,Int)]):List[(Int,Int)] =
    ids match {
      case (a,b)::(c,d)::rest  if(b+2==c && b+1==n) => (a,d)::rest
      case (a,b)::rest =>
        if(n<a-1) (n,n) :: (a,b) :: rest
        else if(a<=n && n<=b) throw new Exception("Already in sequence")
        else if(n==a-1) (n,b)::rest
        else if(n==b+1) (a,n)::rest
        else (a,b)::addToList(n,rest)
      case Nil => List((n,n))
    }
  
  /**
   * Take a specific ID from the middle of the range, and return the 
   * set without that entry
   */
  private def takeFromPool(n:Int,lst:List[(Int,Int)]):List[(Int,Int)] = 
    lst match {
      case (a,b)::rest =>
        if(a==n && b==n) rest
        else if(a==n) (n+1,b)::rest
        else if(b==n) (a,n-1)::rest
        else if(a<=n && n<=b) (a,n-1)::(n+1,b)::rest
        else (a,b)::takeFromPool(n,rest)
      case Nil => throw new Exception("Not in range")
    }
  
  /**
   * Return the ID to the pool, if it's a number.
   */
  def returnToPool(s:Symbol) {
    try {
      var num=s.name.toInt
      ids=addToList(num,ids)
      leased=leased.filter( s != _)
    } catch {
      case e:NumberFormatException => // Do nothing
    }
  }
  
  /**
   * Get the first ID, as a Symbol, from the pool.
   */
  def first(): Symbol = {
    var (id,nids)=takeFirst()
    ids=nids
    leased=id::leased
    id
  }
  
  /**
   * Take a numeric symbol from the pool
   */
  def removeFromPool(s:Symbol) {
    try {
      var num=s.name.toInt
      ids=takeFromPool(num,ids)
      leased=s::leased
    } catch {
      case e:NumberFormatException => // Nothing to do
    }
  }
  
  /**
   * Check if the pool contains a specific id.
   */
  private def poolContains(n:Int,lst:List[(Int,Int)]):Boolean =
    lst match {
      case (a,b)::rest =>
        if(a<=n && n<=b) true
        else poolContains(n,rest)
      case Nil => false
    }
  
  /**
   * Return if s is in the pool, assuming it's a number.
   */
  def contains(s:Symbol):Boolean = {
    try {
      var num=s.name.toInt
      poolContains(num,ids)
    } catch {
      // Nothing to do
      case _ => false
    }
  }
  
  /**
   * Returns the list of all symbols that have been leased.
   */
  def leasedSymbols= leased
  
  override def toString():String = "IDGenerator("+ids+";leased:"+leased + ")"
}