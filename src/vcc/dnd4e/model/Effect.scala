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
    case class RoundBound(id:Symbol,limit:Limit.Value) extends Duration {
      def shortDesc = limit.toString +":"+ id.name
    }
  }
}

object Condition {
  case class Mark(marker:Symbol,permanent:Boolean) extends Condition
  
  case class Generic(desc:String) extends Condition
}

/**
 * This is the father of all conditions
 */
abstract class Condition

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
case class Effect(source:Symbol,condition:Condition,sustainable:Boolean,benefic:Boolean,duration:Effect.Duration) {
}
