/*
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
package vcc.domain.dndi

final object CompendiumCombatantEntityMapper {

  /**
   * Receives a map o simple name -> string, and prefix all the names with either a special prefix or specific prefix
   * @param map Map to be normalized
   * @param defaultPrefix Prefix to be added by default
   * @param prefixMap Name which have a specific prefix, e.g. "name" -> "base" will yield "base:name" -> "The name"
   * @return The map with prefix expanded
   */
  def normalizeAttributeName(map: Map[String, String], defaultPrefix: String, prefixMap: Map[String, String]): Map[String, String] = {
    val newMap = scala.collection.mutable.Map.empty[String,String]
    for((k,v)<-map) {
      newMap += (prefixMap.getOrElse(k, defaultPrefix +":" + k) -> v)
    }
    newMap.toMap
  }

  /**
   * Promotes single names to prefixed name according to CombatantEntity notatios.
   * @param map Unprefixed names to be mapped.
   */
  def normalizeCompendiumNames(map: Map[String, String]): Map[String, String] = normalizeAttributeName(map, "stat", Map(
    "comment" -> "text:comment",
    "description" -> "text:description",
    "level" -> "base:level",
    "name" -> "base:name",
    "role" -> "base:role",
    "race" -> "base:race",
    "class" -> "base:class",
    "type" -> "base:type",
    "xp" -> "base:xp"))

}