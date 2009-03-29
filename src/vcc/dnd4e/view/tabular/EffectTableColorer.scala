//$Id$
package vcc.dnd4e.view.tabular

import vcc.util.swing.ProjectionTableLabelFormatter
import vcc.dnd4e.model.Effect

object EffectTableColorer extends ProjectionTableLabelFormatter[(Symbol,Int,Effect)] {
  import java.awt.Color

  private val attention=(Color.ORANGE,Color.BLACK)
  private val beneficial=(new Color(152,251,152),Color.BLACK)
  private val normal=(Color.WHITE,Color.BLACK)
  
  def setColor(label:javax.swing.JLabel, colors:(Color,Color)) {
    label.setBackground(colors._1)
    label.setForeground(colors._2)
  }
  
  def render(label:javax.swing.JLabel,col:Int,isSelected:Boolean,entry:(Symbol, Int,vcc.dnd4e.model.Effect)) {
	val (tgt,row,eff)=entry
	val needAttention= eff.duration match {
	  case Effect.Duration.RoundBound(who,x,true) => true
	  case Effect.Duration.SaveEndSpecial => true
	  case _ => false	
	}
	setColor(label, col match {
	  case _ if(isSelected) => (label.getBackground,label.getForeground)
	  case 1 if(needAttention)=> attention 
	  case _ if(eff.benefic) =>  beneficial
	  case _ => normal
	})
  }
}
