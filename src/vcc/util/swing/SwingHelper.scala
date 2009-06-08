//$Id$
package vcc.util.swing

object SwingHelper {
  import javax.swing.SwingUtilities
  
  def makeRunnable(f:() =>Unit):java.lang.Runnable =
    new java.lang.Runnable {
      def run() {
        f.apply()
      }
    } 
  
  def invokeLater(f:()=>Unit) {
    javax.swing.SwingUtilities.invokeLater(makeRunnable(f))
  }
  
  def invokeInOtherThread( f: () =>Unit) {
    if(SwingUtilities.isEventDispatchThread()) {
      var run=makeRunnable(f)
      val thr=new Thread(run)
      thr.start()
    } else {
      f
    }
  }
  
  def openDesktopBrowser(url:java.net.URL) {
    try {
      val dsk=java.awt.Desktop.getDesktop
      dsk.browse(url.toURI)
    }catch {
      case _ => println("Failed to open browser on URL: "+url)
    }
  }
}

