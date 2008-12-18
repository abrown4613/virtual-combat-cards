package test

import junit.framework._

import vcc.model._

class HealthTrackingTest extends TestCase {

  def testMinion() {
    
    var minion=new MinionHealthTracker()
    
    // Cant get temporary HP
    minion.setTemporaryHitpoint(10)
    assert(minion.temporaryHP==0)
    
    // Any damage should become one, and cause him to be dead
    minion.applyDamage(10)
    assert(minion.currentHP==0)
    assert(minion.status==HealthStatus.Dead)

    // This should raise de minion
    minion.heal(10)
    assert(minion.currentHP==1)
    assert(minion.status==HealthStatus.Ok)
  }
  
  def testMonster() {
    var monster=new MonsterHealthTracker(45)
    monster.applyDamage(10);
    assert(monster.currentHP==35)
    assert(monster.status==HealthStatus.Ok)
      
    monster.applyDamage(13);
    assert(monster.currentHP==22)
    assert(monster.status==HealthStatus.Bloody)
      
    monster.heal(11)
    assert(monster.currentHP==33)
    assert(monster.status==HealthStatus.Ok)

    monster.heal(20)
    assert(monster.currentHP==monster.hp)
    assert(monster.status==HealthStatus.Ok)
    
    monster.setTemporaryHitpoint(10)
    assert(monster.currentHP==monster.hp)
    assert(monster.temporaryHP==10)
    assert(monster.status==HealthStatus.Ok)
      
    monster.applyDamage(7)
    assert(monster.currentHP==monster.hp)
    assert(monster.temporaryHP==3)
    assert(monster.status==HealthStatus.Ok)

    monster.setTemporaryHitpoint(5)
    assert(monster.currentHP==monster.hp)
    assert(monster.temporaryHP==5)
    assert(monster.status==HealthStatus.Ok)
      
    monster.applyDamage(7)
    assert(monster.currentHP==monster.hp-2)
    assert(monster.temporaryHP==0)
    assert(monster.status==HealthStatus.Ok)
      
    monster.applyDamage(100)
    assert(monster.currentHP==0)
    assert(monster.status==HealthStatus.Dead) 

    monster.heal(10)
    assert(monster.currentHP==10)
    assert(monster.status==HealthStatus.Bloody) 
  }

  def testCharacter() {
    var pc=new CharacterHealthTracker(45)
    pc.applyDamage(10);
    assert(pc.currentHP==pc.hp-10)
    assert(pc.status==HealthStatus.Ok)
      
    pc.applyDamage(13);
    assert(pc.currentHP==(pc.hp-10-13))
    assert(pc.status==HealthStatus.Bloody)
      
    pc.heal(11)
    assert(pc.currentHP==33)
    assert(pc.status==HealthStatus.Ok)

    pc.heal(20)
    assert(pc.currentHP==pc.hp)
    assert(pc.status==HealthStatus.Ok)
    
    pc.setTemporaryHitpoint(10)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==10)
    assert(pc.status==HealthStatus.Ok)
      
    pc.applyDamage(7)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthStatus.Ok)

    pc.setTemporaryHitpoint(2)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthStatus.Ok)

    
    pc.setTemporaryHitpoint(5)
    assert(pc.currentHP==pc.hp)
    assert(pc.temporaryHP==5)
    assert(pc.status==HealthStatus.Ok)
      
    pc.applyDamage(7)
    assert(pc.currentHP==pc.hp-2)
    assert(pc.temporaryHP==0)
    assert(pc.status==HealthStatus.Ok)
      
    pc.applyDamage(50)
    assert(pc.currentHP==pc.hp-52)
    assert(pc.status==HealthStatus.Dying) 

    pc.heal(10)
    assert(pc.currentHP==10)
    assert(pc.status==HealthStatus.Bloody) 
    
    pc.applyDamage(35)
    assert(pc.currentHP== -25)
    assert(pc.status==HealthStatus.Dead) 
  }

  def testCharacterDeath() {
    var pc=new CharacterHealthTracker(40)
    pc.applyDamage(50)
    assert(pc.currentHP==pc.hp-50)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==0)
    
    pc.failDeathSave()
    assert(pc.currentHP==pc.hp-50)
    assert(pc.status==HealthStatus.Dying)
    assert(pc.deathStrikes==1)
    
    pc.heal(25)
    assert(pc.currentHP==25)
    assert(pc.status==HealthStatus.Ok) 

    pc.applyDamage(25)
    assert(pc.currentHP==0)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==0)

    
    pc.failDeathSave()
    assert(pc.currentHP==0)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==1)
    
    pc.applyDamage(5)
    assert(pc.currentHP== -5)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==1)

    pc.failDeathSave()
    assert(pc.currentHP== -5)
    assert(pc.status==HealthStatus.Dying) 
    assert(pc.deathStrikes==2)
  
    pc.failDeathSave()
    assert(pc.currentHP== -5)
    assert(pc.status==HealthStatus.Dead) 

    // Healling will raise PC
    pc.heal(10)
    assert(pc.currentHP==10)
    assert(pc.status==HealthStatus.Bloody) 
    assert(pc.deathStrikes==0)
    
    // Bug make sure can't strike living
    pc.failDeathSave()
    assert(pc.deathStrikes()==0)
    
  }
}
