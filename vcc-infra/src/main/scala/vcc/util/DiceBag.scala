/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package vcc.util

/**
 * Any dice roller or generator should implement this interface.
 */
trait DiceGenerator {
  def D(side: Int): Int

  def flipCoin(): Boolean
}

/**
 * Simple random generator
 */
object DiceBag extends DiceGenerator {
  private val rand = new scala.util.Random()

  /**
   * Set the random seed
   * @param nseed The new seed
   */
  def seed(nseed: Long) {rand.setSeed(nseed)}

  /**
   * Return a random number between 1 and side
   * @param side Number of sides on the dice
   */
  def D(side: Int): Int = rand.nextInt(side) + 1

  /**
   * Generate a true or false
   * @return true or false
   */
  def flipCoin(): Boolean = rand.nextBoolean
}
