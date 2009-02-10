//$Id$
package vcc.controller.actions

/**
 * This is the base class for any action that must be processed within a 
 * transaction.
 */
trait TransactionalAction {
  def description():String
}

/**
 * This is the base class for Query actions.
 */
abstract class QueryAction

case class StartTransaction(desc:String)
case class EndTransaction(desc:String)
case class ClearTransactionLog()

case class Undo() 
case class Redo()

//case class LogError(msg:String)

