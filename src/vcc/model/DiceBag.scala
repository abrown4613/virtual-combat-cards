package vcc.model

object DiceBag {
  private val rand=new scala.util.Random
  
  def seed_=(nseed:Long) { rand.setSeed(nseed) }
  
  def D(side:Int) = { rand.nextInt(side)+1 }
  def flipCoin():Boolean = { rand.nextBoolean }
}
