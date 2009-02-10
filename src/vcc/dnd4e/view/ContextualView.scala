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
  def changeContext(context:Option[T]) 
}
