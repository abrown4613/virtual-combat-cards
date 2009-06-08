//$Id$
package vcc.util

import java.net.URL
import org.xml.sax.InputSource
import scala.xml.XML
import vcc.util.swing.SwingHelper
import vcc.dnd4e.BootStrap

/**
 * Utility functions to help with update manager
 */
object UpdateManager {
  
  /**
   * This is based on Drupal project versioning, we assume all version field are Int
   * except extra (which is a nullable string)
   */
  case class Version(major:Int, minor:Int, patch:Int, extra:String) extends Ordered[Version] {
    def compare(that:Version):Int = {
      var dif=this.major-that.major
      if(dif==0) dif=this.minor-that.minor
      if(dif==0) dif=this.patch-that.patch
      if(dif==0)
        if(this.extra != null && that.extra != null) dif = this.extra.compare(that.extra)
        else if(this.extra!=null) dif = -1
        else if(that.extra!=null) dif = 1
      dif 
    }
 
  	def versionString:String = major +"."+minor+"."+patch+ (if(extra!=null) "-"+extra else "")
   
  	def isPatch(other:Version):Boolean = {
  	  ((this.major==other.major) && (this.minor == other.minor) && this.patch > other.patch)
  	}
  }
  
  /**
   * Case class containing basic information on a release, based on Drupal.org 
   * release-history format
   * @param version A version string, e.g. 0.99.1
   * @param download URL to download file
   * @param md5 MD4 signature for file
   * @param info URL to release note
   */
  case class Release(version:Version, download:URL, md5:String, info:URL) 
  
  //TODO: Maybe get all the release information, since this could be used for a controlled download
  /**
   * Collect version information from a stream, which should point to a XML file. This
   * can be a URL on a remote site. Only published versions should be returned.
   * @param stream The InputSource for the XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(stream: InputSource):Seq[Release] = {
    val release= XML.load(stream)
    val useUnpublished = System.getProperty("vcc.update.unpublished") != null
    
    val releases:Seq[Release] = (release \\ "release").map { release => 
      val major = XMLHelper.nodeSeq2Int(release \ "version_major",0)
      val minor = XMLHelper.nodeSeq2Int(release \ "version_minor",0)
      val patch = XMLHelper.nodeSeq2Int(release \ "version_patch",0)
      val extra = XMLHelper.nodeSeq2String(release \ "version_extra",null)
      val download = XMLHelper.nodeSeq2String(release \ "download_link",null)
      val md5 = XMLHelper.nodeSeq2String(release \ "mdhash",null)
      val info = XMLHelper.nodeSeq2String(release \ "release_link",null)

      if((! useUnpublished && XMLHelper.nodeSeq2String(release \ "status")!= "published") || download == null) null
      else Release(
        Version(major,minor,patch,extra),
          if(download!=null)new URL(download) else null,
          md5,
          if(info!=null)new URL(info) else null
      	)
    }.filter(r=> r != null)
    assert(releases!=null)
    releases
  } 
  
  /**
   * Get version information based on an URL
   * @param URL to the site that contains a Drupal.org release-history XML file
   * @return A valid current Version or null if something went wrong.
   */
  def checkAvailableVersions(url: URL):Seq[Release] = {
    val stream=url.openStream()
	checkAvailableVersions(new InputSource(stream))
  }
  
  /**
   * Download URL to file.
   * @param url URL pointing to the file
   * @param to File to be saved.
   * @return True if downlaod happened correctly
   */
  @deprecated
  def downloadFile(url: URL, to: java.io.File):Boolean = {
    var is:java.io.InputStream=null
    var os:java.io.OutputStream=null
    try {
      is=url.openStream()
      val buf=new Array[Byte](1024)
      if(to.createNewFile) {
        os=new java.io.FileOutputStream(to)
        var sz=is.read(buf)
        while(sz>0) {
          os.write(buf,0,sz)
          sz=is.read(buf)
        }
        os.close()
      }
      is.close()
      to.exists
    } catch {
      case _ =>
        if(is!=null) is.close()
        if(os!=null) os.close()
        if(to.exists) to.delete()
        false
    }
  }
  
  /**
   * Fetch available version then allow user to download choosen versiona 
   * and leave the version ready to update on next launch.
   * @param url URL to fecth available versions
   * @return Success or failure
   */
  def runUpgradeProcess(url:java.net.URL) {

	import vcc.util.swing.multipanel.ReleaseSelectPanel
    import java.io.File
    
    def scanForVersions(afile:File): List[(Symbol,Release)] = {
      val rels=checkAvailableVersions(new InputSource(new java.io.FileInputStream(afile)))
      afile.delete()

      rels.filter(r => { r.version > BootStrap.version}).map({ rel=>
        if(rel.version.extra != null) ('RC,rel)
        else if(rel.version.isPatch(BootStrap.version)) ('PATCH,rel)
        else ('UPGRADE,rel)
      }).toList
	}
 
	def checkFileMD5Sum(file:File, md5sum:String):Boolean = {
	  val chkSum = PackageUtil.fileMD5Sum(file)
	  md5sum.toLowerCase == chkSum.toLowerCase
    }
	val umd= new scala.swing.Frame() with vcc.util.swing.MultiPanel {
        title = "Update Virtual Combat Cards"
        minimumSize= new java.awt.Dimension(300,200)
        iconImage = vcc.dnd4e.view.IconLibrary.MetalD20.getImage
	}
	umd.visible=true
    
	umd.showMessage(false, "Checking for a new version...")

	val afile = umd.downloadFile(url, java.io.File.createTempFile("vcc",".xml") )
    if(afile!=null) {
      var releases=scanForVersions(afile)
      if(releases.length>0) {
    	val releaseOpt=umd.customPanel(new ReleaseSelectPanel(releases))
     
    	if(releaseOpt.isDefined) {	  
          val release=releaseOpt.get
          
          
          val dfile=umd.downloadFile(release.download, java.io.File.createTempFile("vcc",".zip") )
          if(dfile!=null) {
            umd.showMessage(false,"Checkin and unpacking...")
            // We have the file
            if(checkFileMD5Sum(dfile, release.md5)) {
              PackageUtil.extractFilesFromZip(dfile,new File(System.getProperty("user.dir")))
              umd.showMessage(true,"<html><body>Download and extraction completed successfully.<p>To update Virtual Combat Cards, exit and restart it.</body></html>")
            } else {
              umd.showMessage(true,"<html><body>Downloaded file seems to be corrupted. <p> Download and extract manually or report a bug on the site</body></html>")
              SwingHelper.openDesktopBrowser(release.info)
            }
          } else {
        	umd.showMessage(true,"Download failed or cancelled.")
          }
    	} else {
    	  // No version selected
    	}
      } else {
        umd.showMessage(true,"You version is up to date.")
      }
    } else {
      umd.showMessage(true,"<html><body>Failed to download Releases history from:<br>"+url+"</body></html>")
    }
	umd.dispose()
  }

}
