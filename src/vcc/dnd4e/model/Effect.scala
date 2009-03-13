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
    
    /**
     * Round bound duration are durations that change on the limits (startt and end) of
     * rounds.
     * @param id Combatant ID
     * @param limit Round limit (e.g StartOfNextRound, EndOfRound,EndOfNextRound)
     * @param sustain Indicates effect is sustainable
     */
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

  /**
   * Expire effects that end at the rest after combat, this include Stance, EndOfEncounter
   */
  def applyRest():Effect = {
    duration match {
      case Duration.Stance => null
      case Duration.EndOfEncounter => null
      case _ => this
    }
  }
  
  /**
   * If the effect is sustainable, bound duration due to sustain
   */
  def sustain():Effect = {
    duration match {
      case Duration.RoundBound(src,Duration.Limit.EndOfTurn,true) =>
        Effect(source,condition,benefic,Duration.RoundBound(src,Duration.Limit.EndOfNextTurn,true))
      case _ => this
    }
  }
  
  /**
   * Process delay on change of round
   * @param ally Is the delay Combatant and ally of the owner of this effect
   * @param who  Who is delaying
   * @return Effect a changed effect (null if it expired)
   */
  def processDelay(ally:Boolean,who:Symbol):Effect = {
    duration match {
      case Duration.RoundBound(`who`,Duration.Limit.EndOfTurn,true) => null
      case Duration.RoundBound(`who`,Duration.Limit.EndOfTurn,false) if(benefic == ally) => null
      case _ => this
    }
  }
}
