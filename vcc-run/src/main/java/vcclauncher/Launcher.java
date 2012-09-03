/*
 * Copyright (C) 2008-2012 tms - Thomas Santana <tms@exnebula.org>
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
package vcclauncher;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.String;
import java.lang.System;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.FileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.SimpleLayout;

public class Launcher {

  static private Logger _log = null;

  /**
   * @param args
   */
  public static void main(String[] args) {
    String baseDirName = System.getProperty("vcc.install", System.getProperty("user.dir", "."));
    File baseDir = new File(baseDirName);

    _log = startLogger(new File(getLaunchLogPath()));
    _log.info("Starting VCC Lancher 1.1.2");
    _log.debug("----- System Properties -----");
    Enumeration<?> spe = System.getProperties().propertyNames();
    while (spe.hasMoreElements()) {
      Object o = spe.nextElement();
      _log.debug("  " + o + " = " + System.getProperty(o.toString()));
    }
    _log.debug("----- - - - - - - - - - -----");

    try {
      // Check for patchs
      File patch = new File(baseDir, "install.zip");
      if (patch.exists() && patch.isFile()) {
        _log.info("Need to patch VCC, using " + patch.getAbsolutePath());
        extractZipFiles(patch, baseDir);

        if (patch.delete())
          _log.info("Remove the patch indication: " + patch.getAbsolutePath());
        else {
          _log.error("Failed to remove the patch indication: " + patch.getAbsolutePath());
          System.err.println("Failed to remove the patch indication: " + patch.getAbsolutePath());
          _log.error("Please remove the file manually");
          System.err.println("Please remove the file manually");
        }
      }

      // Create loader
      URL[] urls = getLibraryURLs(baseDir);
      if (urls == null) {
        _log.error("Failed to find all need libraries");
        throw new Exception("Failed to load library");
      }
      URLClassLoader ucl = new URLClassLoader(urls, ClassLoader.getSystemClassLoader());

      Class<?> main = ucl.loadClass("vcc.Main");
      Method method = main.getMethod("main", String[].class);
      method.invoke(null, new Object[]{args});
    } catch (Exception e) {
      _log.error("Failed to launch application, reason: " + e.getMessage(), e);
      System.err.println("Failed to launch application, check launch.log in the VCC directory");
    }
  }

  private static void extractZipFiles(File zipFile, File destDir) throws Exception {
    _log.info("Extracting to: " + destDir.getAbsolutePath());
    try {
      byte[] buf = new byte[1024];
      ZipInputStream zipinputstream = null;
      ZipEntry zipentry;
      zipinputstream = new ZipInputStream(new FileInputStream(zipFile));

      zipentry = zipinputstream.getNextEntry();
      while (zipentry != null) {
        //for each entry to be extracted
        String entryName = zipentry.getName();
        _log.info("Extracting " + entryName);
        int n;
        FileOutputStream fileoutputstream;
        File newFile = new File(destDir, entryName);

        if (zipentry.isDirectory()) {
          if (!newFile.exists()) {
            newFile.mkdir();
          } else if (!newFile.isDirectory()) {
            _log.error(newFile.getAbsoluteFile() + " should be a directory");
            throw new RuntimeException(newFile.getAbsoluteFile() + " should be a directory");
          }
        } else {
          File outFile = new File(destDir, entryName);
          fileoutputstream = new FileOutputStream(outFile);

          while ((n = zipinputstream.read(buf, 0, 1024)) > -1)
            fileoutputstream.write(buf, 0, n);

          fileoutputstream.close();
        }
        zipinputstream.closeEntry();
        zipentry = zipinputstream.getNextEntry();

      }//while
      zipinputstream.close();
    } catch (Exception e) {
      e.printStackTrace();
      throw e;
    }
  }

  private static List<String> readFileLines(File file) {
    List<String> strings = new LinkedList<String>();
    try {
      BufferedReader is = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
      String line = is.readLine();
      while (line != null) {
        if (line.charAt(0) != '#') strings.add(line);
        line = is.readLine();
      }
      is.close();
    } catch (FileNotFoundException e) {
      _log.error("Failed to read file: " + file, e);
      return null;
    } catch (IOException e) {
      _log.error("Error while reading file: " + file, e);
      return null;
    }
    return strings;
  }

  private static URL[] getLibraryURLs(File baseDir) throws MalformedURLException {
    File libPath = new File(baseDir, "lib");
    if (!libPath.isDirectory()) {
      _log.error("Can't find library in: " + libPath.getAbsolutePath());
      _log.error("You must run from where VCC is installed, or set 'vcc.install' property");
      return null;
    }
    File libList = new File(libPath, "library.lst");
    File[] files = null;
    if (libList.exists()) {
      _log.debug("Found library file: " + libList);
      List<String> libsNames = readFileLines(libList);
      if (libsNames == null) {
        _log.error("Failed to read library names from " + libList.getAbsolutePath());
        return null;
      }
      files = new File[libsNames.size()];
      int i = 0;
      Iterator<String> iter = libsNames.iterator();
      while (iter.hasNext()) {
        String fname = iter.next();
        File file = new File(libPath, fname);
        _log.debug("Checking library: " + libList);
        if (!file.exists()) {
          _log.error("Could not find required library " + file.getAbsolutePath());
          return null;
        }
        if (!file.canRead()) {
          _log.error("Could not read required library " + file.getAbsolutePath());
          return null;
        }
        files[i] = file;
        i++;
      }
    } else {
      // Read all files in the directory and assume they are jar files
      files = libPath.listFiles();
    }

    URL[] urls = new URL[files.length];
    for (int i = 0; i < files.length; i++) {
      urls[i] = files[i].toURI().toURL();
      _log.info("Library URL: " + urls[i]);
    }
    return urls;
  }

  private static Logger startLogger(File launchLogPath) {
    Logger log = Logger.getLogger("startup");
    log.setLevel(Level.DEBUG);
    SimpleLayout fmt = new SimpleLayout();
    try {
      log.addAppender(new FileAppender(fmt, launchLogPath.toString(), false));
      if (System.getProperty("vcc.debug") != null) log.addAppender(new ConsoleAppender(fmt));
      System.out.println("Logging launch information to: " + launchLogPath.getAbsolutePath());
    } catch (IOException e) {
      log.addAppender(new ConsoleAppender(fmt));
    }
    return log;
  }

  static private String getLaunchLogPath() {
    if (System.getProperty("mrj.version") != null)
      return System.getProperty("user.home") + "/Library/Logs/vcclaunch.log";
    else
      return System.getProperty("user.dir", ".") + File.separator + "vcclaunch.log";
  }
}