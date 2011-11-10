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

package vcc.infra

import org.apache.log4j._
import org.slf4j.{Logger => SLogger}
import vcc.infra.startup.StartupStep

object LogService extends StartupStep {
  private val inDebugMode = (System.getProperty("vcc.console") != null)

  object level extends Enumeration {
    val Debug = Value("DEBUG")
    val Info = Value("INFO")
    val Warn = Value("WARN")
    val Error = Value("ERROR")
    val Fatal = Value("FATAL")
    val Off = Value("OFF")

  }

  protected val mapToLog4J = Map[level.Value, Level](
    level.Debug -> Level.DEBUG,
    level.Info -> Level.INFO,
    level.Warn -> Level.WARN,
    level.Error -> Level.ERROR,
    level.Fatal -> Level.FATAL,
    level.Off -> Level.OFF
    )

  def initializeLog(contexts: Seq[String], filename: String, defaultLevel: level.Value, keep: Boolean) {
    val logger = org.slf4j.LoggerFactory.getLogger("startup")
    val fmt = new org.apache.log4j.TTCCLayout()

    val apdr = if (keep) {
      val lr = new RollingFileAppender(fmt, filename)
      lr.setMaxBackupIndex(10)
      lr.rollOver
      lr
    } else {
      new FileAppender(fmt, filename, false)
    }
    logger.info("Logging to {}", filename)
    for (context <- contexts) {
      if (LogManager.exists(context) == null) {
        val log = Logger.getLogger(context)

        val lvl = level.values.find(_ == System.getProperty("vcc.log." + context)) match {
          case None => defaultLevel
          case Some(l) => l
        }
       
        logger.debug("Log level for {} is {}", context, lvl)
        log.setLevel(mapToLog4J(lvl))

        if (inDebugMode) {
          log.addAppender(new ConsoleAppender(fmt))
        }
        log.addAppender(apdr)
      }
    }
  }

  def initializeStartupLog() {
    val context = "startup"

    if (LogManager.exists(context) == null) {
      val log = Logger.getLogger(context)
      log.setLevel(Level.DEBUG)
      if (inDebugMode) log.addAppender(new ConsoleAppender(new SimpleLayout()))
      log.addAppender(new FileAppender(new SimpleLayout(), "launch.log", false))
    }
  }

  def isStartupComplete = LogManager.exists("infra") != null
}

/**
 * This is a helper object to use to gracefully abort VCC.
 * It should be only used on errors that are absolutely fatal, and reflect
 * something wrong int he program, like missing resources and such.
 */
object AbnormalEnd {
  import java.io._

  def apply(obj: AnyRef, msg: String): Nothing = apply(obj, msg, null)

  def apply(obj: AnyRef, msg: String, e: Throwable): Nothing = {
    def outputMessage(os: PrintStream) {
      os.println("VCC has ended abnormally, this is most likely due to a invalid build")
      if (obj != null) os.println("Reporting object: " + obj.getClass.getCanonicalName)
      os.println("Message: " + msg)
      if (e != null) e.printStackTrace(os)
    }
    try {
      val out = new PrintStream(new FileOutputStream(new File("abort.log")))
      if (out != null) outputMessage(out)
      out.close()
    }
    outputMessage(System.err)
    exit()
  }
}