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
package vcc.dnd4e.view.helper

import scala.util.parsing.combinator._

object DamageParser 	extends JavaTokenParsers {
  abstract class Term {
	def simplify():Term
	def apply(defs:Map[String,Int]):Int
  }

  case class NumberTerm(v:Int) extends Term {
    def simplify():Term = this
    def apply(defs:Map[String,Int]):Int=v
  }
  case class SymbolTerm(s:String) extends Term {
	def simplify():Term = this
	def apply(defs:Map[String,Int]):Int=defs.getOrElse(s,0)  
  }
/*
object Operators extends Enumeration {
  val Add=Value("[+]")
  val Sub=Value("[-]")
  val Mul=Value("[*]")
  val Div=Value("[/]")
}
   */
  case class Op(op:String, l:Term, r:Term) extends Term {
    def simplify():Term = {
      val nl=l.simplify()
      val nr=r.simplify()
      (nl,nr) match {
      	case (NumberTerm(lv),NumberTerm(lr)) => 
      	  op match {
            //case _ => this
            case "*" => NumberTerm(lv * lr)
            case "+" => NumberTerm(lv + lr)
            case "/" => NumberTerm(lv / lr)
            case "-" => NumberTerm(lv - lr)
      	  }
      	case (nl,nr) => Op(op,nl,nr)
      }
    }
    def apply(defs:Map[String,Int]):Int= {
      op match {
        case "+" => l(defs) + r(defs)
        case "-" => l(defs) - r(defs)
        case "*" => l(defs) * r(defs)
        case "/" => l(defs) / r(defs)
      }
    }
    override def toString():String = "{"+l + op + r +"}"
   }

   def reduceListOfOp(list: ~[Term,List[ ~ [ String, Term]]]): Term = {
	 def reduceOp(l:Term,olist: List[ ~ [String,Term]]) :Term = {
	   olist match {
      	 case Nil => l
      	 case op ~ r :: rest => reduceOp(Op(op,l,r),rest)
	   }
	 }
	 list match {
       case v ~ Nil => v
       case v ~ rest => reduceOp(v,rest) 
	 }
   }
  
   def expr: Parser[Term] = term~rep("+"~term | "-"~term) ^^ { reduceListOfOp _ } 
  
   def term: Parser[Term] = factor~rep("*"~factor | "/"~factor) ^^ { reduceListOfOp _ }
  
   def factor: Parser[Term] = wholeNumber ^^ { x=>NumberTerm( x.toInt) } |
      """[sbSB]""".r ^^ {s=>SymbolTerm(s.toLowerCase)} |
      "("~expr~")" ^^ {x => x match { case "("~ e ~ ")"=> e}}
  
   def parseString(str:String):Term = {
     val pr=parseAll(expr,str)
     if(pr.successful) {
       pr.get
     } else {
       throw new Exception("Failed parse"+pr.toString)
     }
   }
}

