//$Id$
package test

import junit.framework._

import vcc.model._


class HealthTrackingTest extends TestCase {

  def checkCharacterDefinitionStatus {
    val hd=CharacterHealthDefinition(21,4,5)
    
    assert(hd.status(HealthTracker(21,0,0,4,hd))==HealthTracker.Status.Ok)
    assert(hd.status(HealthTracker(11,0,0,4,hd))==HealthTracker.Status.Ok)
    assert(hd.status(HealthTracker(10,0,0,4,hd))==HealthTracker.Status.Bloody)
    assert(hd.status(HealthTracker(0,0,0,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,0,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,1,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,2,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,3,4,hd))==HealthTracker.Status.Dead)
    assert(hd.status(HealthTracker(10,0,3,4,hd))==HealthTracker.Status.Dead)
  }
  
  def checkMonsterDefinitionStatus {
    val hd=MonsterHealthDefinition(21,4,5)
    
    assert(hd.status(HealthTracker(21,0,0,4,hd))==HealthTracker.Status.Ok)
    assert(hd.status(HealthTracker(11,0,0,4,hd))==HealthTracker.Status.Ok)
    assert(hd.status(HealthTracker(10,0,0,4,hd))==HealthTracker.Status.Bloody)
    assert(hd.status(HealthTracker(0,0,0,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,0,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,3,4,hd))==HealthTracker.Status.Dead)
  }
  
  def checkMinionDefinitionStatus {
    val hd=MinionHealthDefinition()
    
    assert(hd.status(HealthTracker(1,0,0,4,hd))==HealthTracker.Status.Ok)
    assert(hd.status(HealthTracker(0,0,0,4,hd))==HealthTracker.Status.Dying)
    assert(hd.status(HealthTracker(-5,0,3,4,hd))==HealthTracker.Status.Dead)
  }
  
  
  def testMinion() {
    
    var minion:HealthTracker=HealthTracker.createTracker(CombatantType.Minion,1)
    
    // Cant get temporary HP
    var mmod=minion.setTemporaryHitPoints(10,true)
    assert(mmod eq minion, mmod)
    
    // Any damage should become one, and cause him to be dead
    minion=minion.applyDamage(10)
    assert(minion.currentHP==0)
    assert(minion.status==HealthTracker.Status.Dead)

    // This cant raise anyone
    minion=minion.heal(10)
    assert(minion.currentHP==0)
    assert(minion.status==HealthTracker.Status.Dead)
  }
  
  def testMonster() {
    var monster:HealthTracker=HealthTracker.createTracker(CombatantType.Monster,45)
    monster=monster.applyDamage(10);
    assert(monster.currentHP==35)
    assert(monster.status==HealthTracker.Status.Ok)
      
    monster=monster.applyDamage(13);
    assert(monster.currentHP==22)
    assert(monster.status==HealthTracker.Status.Bloody)
      
    monster=monster.heal(11)
    assert(monster.currentHP==33)
    assert(monster.status==HealthTracker.Status.Ok)

    monster=monster.heal(20)
    assert(monster.currentHP==45)
    assert(monster.status==HealthTracker.Status.Ok)
    
    monster=monster.setTemporaryHitPoints(10,false)
    assert(monster.currentHP==45)
    assert(monster.temporaryHP==10)
    assert(monster.status==HealthTracker.Status.Ok)
      
    monster=monster.applyDamage(7)
    assert(monster.currentHP==monster.base.totalHP)
    assert(monster.temporaryHP==3)
    assert(monster.status==HealthTracker.Status.Ok)

    monster=monster.setTemporaryHitPoints(5,false)
    assert(monster.currentHP==monster.base.totalHP)
    assert(monster.temporaryHP==5)
    assert(monster.status==HealthTracker.Status.Ok)
      
    monster=monster.applyDamage(7)
    assert(monster.currentHP==monster.base.totalHP-2,monster)
    assert(monster.temporaryHP==0)
    assert(monster.status==HealthTracker.Status.Ok)
      
    monster=monster.applyDamage(100)
    assert(monster.currentHP==0,monster)
    assert(monster.status==HealthTracker.Status.Dead) 

    monster=monster.heal(10)
    assert(monster.currentHP==0)
    assert(monster.status==HealthTracker.Status.Dead) 

    monster=monster.raiseFromDead()
    assert(monster.currentHP==0)
    assert(monster.status==HealthTracker.Status.Dying) 
    monster=monster.heal(10)
    assert(monster.currentHP==10)
    assert(monster.status==HealthTracker.Status.Bloody) 

  }

  def testCharacter() {
    var pc:HealthTracker=HealthTracker.createTracker(CombatantType.Character,45)
    pc=pc.applyDamage(10);
    assert(pc.currentHP==pc.base.totalHP-10)
    assert(pc.status==HealthTracker.Status.Ok)
      
    pc=pc.applyDamage(13);
    assert(pc.currentHP==(pc.base.totalHP-10-13))
    assert(pc.status==HealthTracker.Status.Bloody)
      
    pc=pc.heal(11)
    assert(pc.currentHP==33)
    assert(pc.status==HealthTracker.Status.Ok)

    pc=pc.heal(20)
    assert(pc.currentHP==pc.base.totalHP)
    assert(pc.status==HealthTracker.Status.Ok)
    
    pc=pc.setTemporaryHitPoints(10,false)
    assert(pc.currentHP==pc.base.totalHP)
    assert(pc.temporaryHP==10)
    assert(pc.status==HealthTracker.Status.Ok)
      
    pc=pc.applyDamage(7)
    assert(pc.currentHP==pc.base.totalHP)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthTracker.Status.Ok)

    pc=pc.setTemporaryHitPoints(2,false)
    assert(pc.temporaryHP==3)
    assert(pc.status==HealthTracker.Status.Ok)

    
    pc=pc.setTemporaryHitPoints(5,false)
    assert(pc.currentHP==pc.base.totalHP)
    assert(pc.temporaryHP==5)
    assert(pc.status==HealthTracker.Status.Ok)
      
    pc=pc.applyDamage(7)
    assert(pc.currentHP==pc.base.totalHP-2)
    assert(pc.temporaryHP==0)
    assert(pc.status==HealthTracker.Status.Ok)
      
    pc=pc.applyDamage(50)
    assert(pc.currentHP==pc.base.totalHP-52)
    assert(pc.status==HealthTracker.Status.Dying) 

    pc=pc.heal(10)
    assert(pc.currentHP==10)
    assert(pc.status==HealthTracker.Status.Bloody) 
    
    pc=pc.applyDamage(35)
    assert(pc.currentHP== -22,pc)
    assert(pc.status==HealthTracker.Status.Dead) 
  }

  def testCharacterDeath() {
    var pc=HealthTracker.createTracker(CombatantType.Character,40)

    // Bug make sure can't strike living
    pc=pc.failDeathSave()
    assert(pc.deathStrikes==0)
    

    pc=pc.applyDamage(50)
    assert(pc.currentHP==40-50)
    assert(pc.status==HealthTracker.Status.Dying) 
    assert(pc.deathStrikes==0)
    
    pc=pc.failDeathSave()
    assert(pc.currentHP==40-50)
    assert(pc.status==HealthTracker.Status.Dying)
    assert(pc.deathStrikes==1)
    
    pc=pc.heal(25)
    assert(pc.currentHP==25)
    assert(pc.status==HealthTracker.Status.Ok) 

    pc=pc.applyDamage(25)
    assert(pc.currentHP==0)
    assert(pc.status==HealthTracker.Status.Dying) 
    assert(pc.deathStrikes==1)

    
    pc=pc.failDeathSave()
    assert(pc.currentHP==0)
    assert(pc.status==HealthTracker.Status.Dying) 
    assert(pc.deathStrikes==2)
    
    pc=pc.applyDamage(5)
    assert(pc.currentHP== -5)
    assert(pc.status==HealthTracker.Status.Dying) 
    assert(pc.deathStrikes==2)

    pc=pc.failDeathSave()
    assert(pc.currentHP== -5)
    assert(pc.deathStrikes==3)
    assert(pc.status==HealthTracker.Status.Dead) 

    // Healling will not raise PC
    pc=pc.heal(10)
    assert(pc.currentHP== -5,pc)
    assert(pc.status==HealthTracker.Status.Dead) 
    assert(pc.deathStrikes==3)
    
  }
  
  def testUnboundHealBug() {
    var pc=HealthTracker.createTracker(CombatantType.Character,20)
    pc=pc.applyDamage(25)
    assert(pc.currentHP== -5)
    pc=pc.heal(300)
    assert(pc.currentHP==20,pc)
  }
  
  
  def testRaiseDeadLogic() {
    var pc=HealthTracker.createTracker(CombatantType.Character,20)
    pc=pc.applyDamage(30)
    assert(pc.status==HealthTracker.Status.Dead)
    pc=pc.raiseFromDead()
    assert(pc.currentHP==0)
    assert(pc.status!=HealthTracker.Status.Dead)
    
    var monst=HealthTracker.createTracker(CombatantType.Monster,20)
    monst=monst.applyDamage(20);
    assert(monst.status==HealthTracker.Status.Dead)
    monst=monst.raiseFromDead()
    assert(monst.currentHP==0)
    assert(monst.status!=HealthTracker.Status.Dead)

    monst=HealthTracker.createTracker(CombatantType.Minion,20)
    monst=monst.applyDamage(20);
    assert(monst.status==HealthTracker.Status.Dead)
    monst=monst.raiseFromDead()
    assert(monst.currentHP==0)
    assert(monst.status!=HealthTracker.Status.Dead)

  }
  
  def testRestLogic() {
    var pc=HealthTracker.createTracker(CombatantType.Character,20)
    
    //Make him dying
    pc=pc.applyDamage(25)
    pc=pc.failDeathSave()
    assert(pc.status == HealthTracker.Status.Dying)
    assert(pc.deathStrikes == 1)
    
    // Fork to a dead by strikes
    var dpc=pc.failDeathSave()
    dpc=dpc.failDeathSave()
    assert(dpc.deathStrikes==3)
    assert(dpc.status == HealthTracker.Status.Dead)
    
    // No more strikes but still dying
    val pc_short=pc.rest(false)
    assert(pc_short.currentHP==pc.currentHP)
    assert(pc_short.deathStrikes==0)
    assert(pc_short.status==HealthTracker.Status.Dying)

    // Dying PC should go back to ok
    val pc_extended=pc.rest(true)
    assert(pc_extended.currentHP==pc_extended.base.totalHP)
    assert(pc_extended.deathStrikes==0)
    assert(pc_extended.status==HealthTracker.Status.Ok)
    
    // Dead will not improve, and not change at all
    var ndpc=dpc.rest(false)
    assert(ndpc eq dpc)
    assert(ndpc.deathStrikes==3)
    assert(ndpc.status == HealthTracker.Status.Dead)
    
    // Dead still dead after long rest
    ndpc=dpc.rest(true)
    assert(ndpc eq dpc)
    assert(ndpc.deathStrikes==3)
    assert(ndpc.status == HealthTracker.Status.Dead)
    
  }
  
}
