//$Id$
package vcc.util

/**
 * Simple random generator
 */
object DiceBag {
  private val rand=new scala.util.Random
  
  /**
   * Set the random seed
   * @param nseed The new seed
   */
  def seed(nseed:Long) { rand.setSeed(nseed) }
  
  /**
   * Return a random number between 1 and side
   * @param side Number of sides on the dice
   */
  def D(side:Int):Int = rand.nextInt(side)+1
  
  /**
   * Generate a true or false
   * @return true or false
   */
  def flipCoin():Boolean = rand.nextBoolean
}
