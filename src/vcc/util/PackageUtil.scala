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
//$Id$
package vcc.util

import java.io._
import java.util.zip._
import java.security.MessageDigest

object PackageUtil {
  def fileMD5Sum(file:File):String =  {
	val digest = MessageDigest.getInstance("MD5");
	val buffer = new Array[Byte](8192);
	var read = 0;
	val is=new FileInputStream(file)
	var md5text:String = null
	try {
	  read = is.read(buffer)
	  while( read  > 0) {
		digest.update(buffer, 0, read);
		read = is.read(buffer)
	  }		
	  val md5sum = digest.digest();
	  val bigInt = new java.math.BigInteger(1, md5sum);
	  md5text = bigInt.toString(16);
	} catch {
	  case e:java.io.IOException =>
	    is.close()
		throw new RuntimeException("Unable to process file for MD5", e);
	}
	try {
		is.close();
	} catch {
	  case e:java.io.IOException =>
			throw new RuntimeException("Unable to close input stream for MD5 calculation", e);
	}
	md5text
  }
  
  def extractFilesFromZip(zipFile:File, destdir:File) {
    try {
      if(!destdir.exists && !destdir.isDirectory) {
        throw new Exception("Bad directory "+destdir)
      }
                          
      
      val buf = new Array[Byte](1024)
      var zipentry:ZipEntry = null
      val zipinputstream = new ZipInputStream(new FileInputStream(zipFile))

      zipentry = zipinputstream.getNextEntry();
      while (zipentry != null) { 
    	//for each entry to be extracted
        val entryName = zipentry.getName()
        System.out.println("Extracting file: "+entryName)
        var n:Int=0
        val newFile = new File(entryName);
        val directory = newFile.getParent();
        
        val dest=new File(destdir,entryName)
        if(zipentry.isDirectory()) {
        	// TO Nothing
          println("Creating directory: "+entryName)
          if(!dest.exists) dest.mkdir()
          if(dest.exists && !dest.isDirectory) throw new Exception("Zip contains directory "+entryName + " which is not a directory")
        } else {
          val fileoutputstream = new FileOutputStream(dest)             
        
          n = zipinputstream.read(buf, 0, 1024)
          while ((n) > -1) {
            fileoutputstream.write(buf, 0, n);
            n = zipinputstream.read(buf, 0, 1024)
          }
          fileoutputstream.close(); 
        }
        zipinputstream.closeEntry();
        zipentry = zipinputstream.getNextEntry();
      }//while

      zipinputstream.close();
   } catch { 
     case e:Exception =>
     	e.printStackTrace();
   }
  }
  
}
