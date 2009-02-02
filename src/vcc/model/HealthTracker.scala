//$Id$
package vcc.model

/**
 * HeatlthTracker master object
 */
object HealthTracker {
  
  /**
   * TypeSemantic captures combatant type (minion, monster, character) specific semantics
   * for handling Temporary HP, status, and bounds
   */
  trait TypeSemantic{
    val hasTemporaryHP:Boolean
    def lowerBound(hp:Int):Int
    def status(tracker:HealthTracker):HealthStatus.Value
  }
  
  /**
   * Handle minions
   */
  object MinionSemantic extends TypeSemantic {
    val hasTemporaryHP:Boolean=false
    def lowerBound(hp:Int)=0
    def status(tracker:HealthTracker):HealthStatus.Value = {
      if(tracker.currentHP<=0) HealthStatus.Dead else HealthStatus.Ok
    }
  } 
  
  object MonsterSemantic extends TypeSemantic  {
    val hasTemporaryHP:Boolean=true
    def lowerBound(hp:Int)=0
    def status(tracker:HealthTracker):HealthStatus.Value= {
      if(tracker.currentHP<=0) HealthStatus.Dead
      else if(tracker.currentHP<=tracker.hp/2) HealthStatus.Bloody
      else HealthStatus.Ok
    }
  }
  
  object CharacterSemantic extends TypeSemantic  {
    val hasTemporaryHP:Boolean=true
    def lowerBound(hp:Int):Int= - hp / 2
    def status(tracker:HealthTracker):HealthStatus.Value= {
      if(tracker.deathStrikes>=3 || tracker.currentHP == lowerBound(tracker.hp)) HealthStatus.Dead
      else if(tracker.currentHP <= 0) HealthStatus.Dying
      else if(tracker.currentHP<=tracker.hp/2) HealthStatus.Bloody
      else HealthStatus.Ok
    }	
  }
  
  def createTracker(ctype:CombatantType.Value, hp:Int):HealthTracker = {
    HealthTracker(hp,hp,0,0,1,ctype match {
      case CombatantType.Monster => MonsterSemantic
      case CombatantType.Minion => MinionSemantic
      case CombatantType.Character => CharacterSemantic
    })
  }
}

/*
 * Since character are the most complex, this class implements their logic, 
 * Other creatures will have different sublogics
 */
case class HealthTracker(hp:Int,currentHP:Int,temporaryHP:Int,deathStrikes:Int,surges:Int, seman:HealthTracker.TypeSemantic) {

  private def boundedChange(amnt:Int):Int = { 
    val n=currentHP+amnt;
    if(n<seman.lowerBound(hp)) seman.lowerBound(hp)
    else if(n>hp) hp
    else n
  }
  
  def applyDamage(amnt:Int) = {
    if(amnt>temporaryHP) {
      HealthTracker(hp,boundedChange(-amnt+temporaryHP),0,deathStrikes,surges,seman)
    } else { 
      HealthTracker(hp,currentHP,temporaryHP-amnt,deathStrikes,surges,seman)
    }
  }
  
  def heal(amnt:Int) = {
    HealthTracker(hp,boundedChange(if(currentHP<0)amnt-currentHP else amnt),temporaryHP,deathStrikes,surges,seman)
  }
  
  def setTemporaryHitPoints(amnt:Int,force:Boolean) = {
    if(seman.hasTemporaryHP) {
      if(force) HealthTracker(hp,currentHP,amnt,deathStrikes,surges,seman)
      else if(amnt>temporaryHP)
        HealthTracker(hp,currentHP,amnt,deathStrikes,surges,seman)
      else this
    } else this
  }
  
  def failDeathSave():HealthTracker={ 
    if(this.status == HealthStatus.Dying)
      HealthTracker(hp,currentHP,temporaryHP,deathStrikes+1,surges,seman)
    else this
  }
  
  def status():HealthStatus.Value=seman.status(this) 
}