//$Id$
package test

import junit.framework._

import vcc.model._

class HealthTrackingTest extends TestCase {

  def testMinion() {
    
    var minion:HealthTracker=HealthTracker.createTracker(CombatantType.Minion,1)
    
    // Cant get temporary HP
    var mmod=minion.setTemporaryHitPoints(10,true)
    assert(mmod eq minion, mmod)
    
    // Any damage should become one, and cause him to be dead
    minion=minion.applyDamage(10)
    assert(minion.currentHP==0)
    assert(minion.status==HealthStatus.Dead)

    // This should raise de minion
    minion=minion.heal(10)
    assert(minion.currentHP==1)
    assert(minion.status==HealthStatus.Ok)
  }
  
  def testMonster() {
    var monster:HealthTracker=HealthTracker.createTracker(CombatantType.Monster,45)
    monster=monster.applyDamage(10);
    assert(monster.currentHP==35)
    assert(monster.status==HealthStatus.Ok)
      
    monster=monster.applyDamage(13);
    assert(monster.currentHP==22)
    assert(monster.status==HealthStatus.Bloody)
      
    monster=monster.heal(11)
    assert(monster.currentHP==33)
    assert(monster.status==HealthStatus.Ok)

    monster=monster.heal(20)
    assert(monster.currentHP==45)
    assert(monster.status==HealthStatus.Ok)
    
    monster=monster.setTemporaryHitPoints(10,false)
    assert(monster.currentHP==45)
    assert(monster.temporaryHP==10)
    assert(monster.status==HealthStatus.Ok)
      
    monster=monster.applyDamage(7)
    assert(monster.currentHP==monster.hp)
    assert(monster.temporaryHP==3)
    assert(monster.status==HealthStatus.Ok)

    monster=monster.setTemporaryHitPoints(5,false)
    assert(monster.currentHP==monster.hp)
    assert(monster.temporaryHP==5)
    assert(monster.status==HealthStatus.Ok)
      
    monster=monster.applyDamage(7)
    assert(monster.currentHP==monster.hp-2,monster)
    assert(monster.temporaryHP==0)
    assert(monster.status==HealthStatus.Ok)
      
    monster=monster.applyDamage(100)
    assert(monster.currentHP==0,monster)
    assert(monster.status==HealthStatus.Dead) 

    monster=monster.heal(10)
    assert(monster.currentHP==10)
    assert(monster.status==HealthStatus.Bloody) 
  }

  def testCharacter() {
    var pc:HealthTracker=HealthTracker.createTracker(CombatantType.Character,45)
    pc=pc.applyDamage(10);
    assert(pc.currentHP==pc.hp-10)
    assert(pc.status==HealthStatus.Ok)
      
    pc=pc.applyDamage(13);
    assert(pc.currentHP==(pc.hp-10-13))
    assert(pc.status==HealthStatus.Bloody)
      
    pc=pc.heal(11)
    assert(pc.currentHP==33)
    assert(pc.status==HealthStatus.Ok)

    pc=pc.heal(20)
    assert(pc.currentHP==pc.hp)
    assert(pc.status==HealthStatus.Ok)
    
    pc=pc.setTemporaryHitPoints(10,false)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==10)
    assert(pc.status==HealthStatus.Ok)
      
    pc=pc.applyDamage(7)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthStatus.Ok)

    pc=pc.setTemporaryHitPoints(2,false)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthStatus.Ok)

    
    pc=pc.setTemporaryHitPoints(5,false)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==5)
    assert(pc.status==HealthStatus.Ok)
      
    pc=pc.applyDamage(7)
    assert(pc.currentHP==pc.hp-2)
    assert(pc.temporaryHP==0)
    assert(pc.status==HealthStatus.Ok)
      
    pc=pc.applyDamage(50)
    assert(pc.currentHP==pc.hp-52)
    assert(pc.status==HealthStatus.Dying) 

    pc=pc.heal(10)
    assert(pc.currentHP==10)
    assert(pc.status==HealthStatus.Bloody) 
    
    pc=pc.applyDamage(35)
    assert(pc.currentHP== -22,pc)
    assert(pc.status==HealthStatus.Dead) 
  }

  def testCharacterDeath() {
    var pc=HealthTracker.createTracker(CombatantType.Character,40)

    // Bug make sure can't strike living
    pc=pc.failDeathSave()
    assert(pc.deathStrikes==0)
    

    pc=pc.applyDamage(50)
    assert(pc.currentHP==40-50)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==0)
    
    pc=pc.failDeathSave()
    assert(pc.currentHP==40-50)
    assert(pc.status==HealthStatus.Dying)
    assert(pc.deathStrikes==1)
    
    pc=pc.heal(25)
    assert(pc.currentHP==25)
    assert(pc.status==HealthStatus.Ok) 

    pc=pc.applyDamage(25)
    assert(pc.currentHP==0)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==1)

    
    pc=pc.failDeathSave()
    assert(pc.currentHP==0)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==2)
    
    pc=pc.applyDamage(5)
    assert(pc.currentHP== -5)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==2)

    pc=pc.failDeathSave()
    assert(pc.currentHP== -5)
    assert(pc.deathStrikes==3)
    assert(pc.status==HealthStatus.Dead) 

    // Healling will raise PC
    pc=pc.heal(10)
    assert(pc.currentHP==10)
    assert(pc.status==HealthStatus.Dead) 
    assert(pc.deathStrikes==3)
    
  }
  
  def testUnboundHealBug() {
    var pc=HealthTracker.createTracker(CombatantType.Character,20)
    pc=pc.applyDamage(25)
    assert(pc.currentHP== -5)
    pc=pc.heal(300)
    assert(pc.currentHP==20,pc)
  }
}
