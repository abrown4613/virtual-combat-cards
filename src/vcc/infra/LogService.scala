//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

package vcc.infra

import org.apache.log4j._
import org.slf4j.{Logger => SLogger}

object LogService {
  
  object level extends Enumeration {
    val Debug = Value("Debug")
    val Info = Value("Info")
    val Error = Value("Error")
    val Fatal = Value("Fatal")
    val Off = Value("Off")
    
  }
  
  protected val mapToLog4J = Map[level.Value,Level](
    level.Debug -> Level.DEBUG,
    level.Info -> Level.INFO,
    level.Error -> Level.ERROR,
    level.Fatal -> Level.FATAL,
    level.Off -> Level.OFF
  )
  
  def initializeLog(context:String, lvl: level.Value) {
	val log = Logger.getLogger(context)
	
    log.setLevel(mapToLog4J(lvl))
    val fmt = new org.apache.log4j.TTCCLayout()// SimpleLayout()
    val apdr = new org.apache.log4j.ConsoleAppender(fmt)
    log.addAppender(apdr)
  }
  
}
