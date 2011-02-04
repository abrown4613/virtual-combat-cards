/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common

import annotation.tailrec

/**
 * Helper functions and extractors to analyse condition text.
 */
object ConditionMatcher {

  /**
   * Returns the first Regenerate condition in the condition text.
   */
  object FirstRegenerate {
    private val regenRE = """(?i)(regen\w*\s+(\d+).*)(?:and)?""".r

    @tailrec
    private def findSubCondition(l: List[String]): Option[(String, Int)] = {
      l match {
        case Nil => None
        case this.regenRE(full, hint) :: rest => Some((full, hint.toInt))
        case head :: rest => findSubCondition(rest)
      }
    }

    /**
     * Returns the first Regenerate condition in the condition text.
     */
    def unapply(text: String): Option[(String, Int)] = {
      findSubCondition(splitFirstCondition(text))
    }
  }

  /**
   * Returns the first Ongoing sub condition in the condition text.
   */
  object FirstOngoing {
    private val ongoingRE = ("""(?i)(ongoing\s+(\d+).*)(?:and)?""".r)

    @tailrec
    private def findSubCondition(l: List[String]): Option[(String, Int)] = {
      l match {
        case Nil => None
        case this.ongoingRE(full, hint) :: rest => Some((full, hint.toInt))
        case head :: rest => findSubCondition(rest)
      }
    }

    /**
     * Returns the first Ongoing sub condition in the condition text.
     */
    def unapply(text: String): Option[(String, Int)] = {
      findSubCondition(splitFirstCondition(text))
    }
  }

  /**
   * Split condition on the <b>and</b> word.
   */
  def splitFirstCondition(text: String): List[String] = {
    splitProgression(text)(0).split("""(?i)\s*(?:and)\s*""").toList
  }

  /**
   * Split Condition by the progression indicators (-> or / ).
   */
  def splitProgression(text: String) = text.split("""\s*(?:\->|\/)\s*""")
}
