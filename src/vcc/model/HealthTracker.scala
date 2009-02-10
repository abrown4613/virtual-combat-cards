//$Id$
package vcc.dnd4e.model

/**
 * Health base definition for the combatant
 * @param totalHP The total Hitpoints
 * @param surgeValue Surge value normally 1/4 of HP
 * @param healingSurges Number os surges
 */
abstract case class HealthDefinition(totalHP:Int,surgeValue:Int,healingSurges:Int) {
  val lowerBound:Int
  val hasTemporaryHP:Boolean
  def status(tracker:HealthTracker):HealthTracker.Status.Value
  val ctype:CombatantType.Value
} 

case class CharacterHealthDefinition(override val totalHP:Int,override val surgeValue:Int,override val healingSurges:Int) extends HealthDefinition(totalHP,surgeValue,healingSurges) {
  val lowerBound= - totalHP/2
  val hasTemporaryHP = true
  val ctype=CombatantType.Character
  def status(tracker:HealthTracker)= {
    if(tracker.deathStrikes==3) HealthTracker.Status.Dead
    else if(tracker.currentHP<=0) HealthTracker.Status.Dying
    else if(tracker.currentHP<=totalHP/2) HealthTracker.Status.Bloody
    else HealthTracker.Status.Ok
  }
}

case class MonsterHealthDefinition(override val totalHP:Int,override val surgeValue:Int,override val healingSurges:Int) extends CharacterHealthDefinition(totalHP,surgeValue,healingSurges) {
  override val lowerBound=0
  override val ctype=CombatantType.Monster
}

case class MinionHealthDefinition() extends HealthDefinition(1,0,0) {
  val lowerBound=0
  val ctype=CombatantType.Minion
  override val hasTemporaryHP = false

  def status(tracker:HealthTracker)= {
    if(tracker.deathStrikes==3) HealthTracker.Status.Dead
    else if(tracker.currentHP<=0) HealthTracker.Status.Dying
    else HealthTracker.Status.Ok
  }
}

/**
 * HeatlthTracker master object
 */
object HealthTracker {
  
  object Status extends Enumeration {
    val Ok=Value("Ok")
    val Bloody=Value("Bloody")
    val Dead=Value("Dead")
    val Dying=Value("Dying")
  }
  
  /**
   * Create a HealthTracker based on a HealthDefinition
   * @param hdef Base health definition
   */
  def createTracker(hdef:HealthDefinition):HealthTracker = {
    HealthTracker(hdef.totalHP,0,0,hdef.healingSurges,hdef)
  }

  /**
   * Create a HealthTracker based on a HealthDefinition
   * @param hdef Base health definition
   */
  @deprecated
  def createTracker(ctype:CombatantType.Value,hp:Int):HealthTracker = {
    ctype match {
      case CombatantType.Character => createTracker(CharacterHealthDefinition(hp,hp/4,4))
      case CombatantType.Monster => createTracker(MonsterHealthDefinition(hp,hp/4,4))
      case CombatantType.Minion => createTracker(MinionHealthDefinition())
    }
  }
  
}

/*
 * Since character are the most complex, this class implements their logic, 
 * Other creatures will have different sublogics
 */
case class HealthTracker(currentHP:Int,temporaryHP:Int,deathStrikes:Int,surges:Int,base:HealthDefinition) {
  
  private def boundedChange(amnt:Int):Int = { 
    val n=currentHP+amnt;
    if(n<base.lowerBound) base.lowerBound
    else if(n>base.totalHP) base.totalHP
    else n
  }
  
  def applyDamage(amnt:Int) = {
    if(amnt>temporaryHP) {
      val nchp=boundedChange(-amnt+temporaryHP)
      HealthTracker(nchp,0,if(nchp==base.lowerBound) 3 else deathStrikes,surges,base)
    } else { 
      HealthTracker(currentHP,temporaryHP-amnt,deathStrikes,surges,base)
    }
  }
  
  /**
   * Healing dead is not allowed. They most be removed from death before updating hp
   */
  def heal(amnt:Int) = {
    if(status != HealthTracker.Status.Dead)
      HealthTracker(boundedChange(if(currentHP<0)amnt-currentHP else amnt),temporaryHP,deathStrikes,surges,base)
    else 
      this
  }
  
  def setTemporaryHitPoints(amnt:Int,force:Boolean) = {
    if(base.hasTemporaryHP) {
      if(force) HealthTracker(currentHP,amnt,deathStrikes,surges,base)
      else if(amnt>temporaryHP)
        HealthTracker(currentHP,amnt,deathStrikes,surges,base)
      else this
    } else this
  }
  
  def failDeathSave():HealthTracker={ 
    if(this.status == HealthTracker.Status.Dying)
      HealthTracker(currentHP,temporaryHP,deathStrikes+1,surges,base)
    else this
  }
  
  def status()=base.status(this) 

  def rest(extended:Boolean):HealthTracker = {
    if(status!=HealthTracker.Status.Dead) {
      if(extended) HealthTracker(base.totalHP,0,0,1,base)
      else HealthTracker(currentHP,0,0,1,base)
    } else 
      this
  }

  def raiseFromDead():HealthTracker = {
    if(status==HealthTracker.Status.Dead) {
      HealthTracker(0,temporaryHP,0,surges,base)
    } else this
  }
}