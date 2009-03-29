//$Id$
package vcc.dnd4e.view

import vcc.util.swing.ProjectionTableLabelFormatter

object ViewCombatantTableColorer extends ProjectionTableLabelFormatter[ViewCombatant] {
  import java.awt.Color
  import vcc.dnd4e.model.InitiativeState._
  import vcc.dnd4e.model.HealthTracker.Status._
  
  private val cellFont=new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN,14)

  // Pair[Color,Color]  where (background,foreground)
  private val grayed=(Color.LIGHT_GRAY,Color.BLACK)
  private val dead=(Color.BLACK,Color.WHITE)
  private val dying=(Color.GRAY,Color.WHITE)
  private val bloody=(new Color(220,20,60),Color.WHITE)
  private val ready=(Color.ORANGE,Color.BLACK)
  private val normal=(Color.WHITE,Color.BLACK)
  private val selected=(Color.BLUE,Color.WHITE)
  
  private def setColor(label:javax.swing.JLabel, cp:Pair[Color,Color]):Unit = {
    label.setBackground(cp._1)
    label.setForeground(cp._2)
  }
  
  def render(label:javax.swing.JLabel, col:Int, isSelected:Boolean, cmb: ViewCombatant):Unit = {
    var is=cmb.initTracker.state
    var hs=cmb.health.status
    label.setFont(cellFont)
    label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
    setColor(label,col match {
      case 0 if(is==Ready || is==Delaying)=> ready
      case 3 =>
         hs match {
           case Dead => dead
           case Dying => dying
           case Bloody => bloody
           case _ if(isSelected) => (label.getBackground,label.getForeground)
           case _ if(is==Reserve) => grayed
           case _ => normal
         }
      case 5 if(is==Ready || is==Delaying)=> ready
      case _ if(isSelected) => (label.getBackground,label.getForeground)
      case _ if(hs==Dead)=> grayed 
      case _ if(is==Reserve)=> grayed 
      case _ => normal
    })
  }
}
