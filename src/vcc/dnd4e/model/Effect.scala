//$Id$
package vcc.dnd4e.model

object Effect {
  
  /**
   * Determines the duration of an effect.
   */
  abstract class Duration {
    def shortDesc:String
    
    override def toString()="Effect.Duration("+shortDesc+")"
  }
  
  object Duration {
    
    object SaveEnd extends Duration {
      val shortDesc="SE"
    }
    
    object SaveEndSpecial extends Duration {
      val shortDesc="SE*"
    }

    object Stance extends Duration {
      val shortDesc="Stance"
    }
    
    object EndOfEncounter extends Duration {
      val shortDesc="EoE"
    }
    
    object Other extends Duration {
      val shortDesc="Other"
    }
    
    object Limit extends Enumeration {
      val EndOfNextTurn=Value("EoNT")
      val EndOfTurn=Value("EoT")
      val StartOfNextTurn=Value("SoNT")
    }
    
    case class RoundBound(id:Symbol,limit:Limit.Value,sustain:Boolean) extends Duration {
      def shortDesc = limit.toString + (if(sustain) "*" else "") +":"+ id.name
    }
  }
}

object Condition {
  
  case class Mark(marker:Symbol,permanent:Boolean) extends Condition {
    def description="Marked by "+marker.name+(if(permanent) " no mark can supersede" else "")
  }
  
  case class Generic(description:String) extends Condition
}

/**
 * This is the father of all conditions
 */
abstract class Condition {
  def description:String
}

/**
 * Effect representa a condition being applied to a target for a set duration. The proper
 * model should be a set of Conditions, but for the time being one will do. Effect have the
 * following:
 * @param source The symbol of the source of the effect, used to determine alliedness for delay
 * @param condition The condition being applied
 * @param sustainable Indicates power can be sustained
 * @param benefic Indicates if power is good for the target (important for delay)
 * @param duaration An Effect.Duration
 */
case class Effect(source:Symbol,condition:Condition,benefic:Boolean,duration:Effect.Duration) {
  import Effect._
  
  def sustainable=duration match {
    case Effect.Duration.RoundBound(c,l,true) => true
    case _ => false
  }
  
  /**
   * Change duration according to the start of a round of some combatant
   */
  def startRound(cid:Symbol):Effect = {
    duration match {
      case Duration.RoundBound(`cid`,Duration.Limit.StartOfNextTurn,sust) => null
      case Duration.RoundBound(`cid`,Duration.Limit.EndOfNextTurn,sust) =>
        Effect(source,condition,benefic,Duration.RoundBound(cid,Duration.Limit.EndOfTurn,sust))
      case _ => this
    }
  }

  /**
   * Change duration according to the start of a round of some combatant
   */
  def endRound(cid:Symbol):Effect = {
    duration match {
      case Duration.RoundBound(`cid`,Duration.Limit.EndOfTurn,sust) => null
      case _ => this
    }
  }
}
