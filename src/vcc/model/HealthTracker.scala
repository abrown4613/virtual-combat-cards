package vcc.model

case class HealthTrackerSummary(currhp:Int,temphp:Int,status:HealthStatus.Value,deathstrikes:Int)

/*
 * Since character are the most complex, this class implements their logic, 
 * Other creatures will have different sublogics
 */
abstract class HealthTracker(val hp:Int) {
  var _currhp:Int=hp
  var _bloody:Int=hp/2
  var _temphp:Int=0
  var _deathStrikes:Int=0

  
  def status():HealthStatus.Value
  def currentHP():Int = _currhp
  def temporaryHP():Int= _temphp
  
  def failDeathSave():Unit={ if(this.status == HealthStatus.Dying)_deathStrikes+=1}
  def deathStrikes():Int=_deathStrikes
  def setTemporaryHitpoint(hp:Int) = if(hp>_temphp) _temphp=hp
  def applyDamage(amnt:Int)= {
    // Adjust for temp hp
    var adj_amnt=amnt-_temphp
    if(adj_amnt<0) adj_amnt=0
    
    // Remove from temphp what it took out and then adjust damage
    _temphp-=(amnt-adj_amnt)
    _currhp-=adj_amnt
  }	
  def heal(amnt:Int) = {
    // raise to 0 before healing
    if(_currhp<0) _currhp=0
    _currhp+=amnt; 
    if(_currhp>hp) _currhp=hp
    _deathStrikes=0
  }
  
  def getSummary():HealthTrackerSummary=HealthTrackerSummary(this.currentHP,this.temporaryHP,this.status,this.deathStrikes)
}

class MinionHealthTracker extends MonsterHealthTracker(1) {
  override def status():HealthStatus.Value=if(_currhp<=0) HealthStatus.Dead else HealthStatus.Ok
  override def setTemporaryHitpoint(x:Int)={}
}

class MonsterHealthTracker(hp:Int) extends HealthTracker(hp) {
  def status():HealthStatus.Value= {
    if(_currhp<=0) HealthStatus.Dead
    else if(_currhp<=_bloody) HealthStatus.Bloody
    else HealthStatus.Ok
  }
  override def applyDamage(amnt:Int)= {super.applyDamage(amnt); if(_currhp<0) _currhp=0}
}

class CharacterHealthTracker(hp:Int) extends HealthTracker(hp) {
  def status():HealthStatus.Value= {
    if(_deathStrikes>=3 || _currhp <= -_bloody) HealthStatus.Dead
    else if(_currhp <= 0) HealthStatus.Dying
    else if(_currhp<=_bloody) HealthStatus.Bloody
    else HealthStatus.Ok
  }
}