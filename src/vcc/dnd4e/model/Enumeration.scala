//$Id$
package vcc.dnd4e.model

object InitiativeState extends Enumeration {
  val Reserve=Value("Reserve")
  val Ready=Value("Ready")
  val Readying=Value("Readying") // This state is when the combatant has readied but not ended it's round
  val Surprised=Value("Surprised")
  val Delaying=Value("Delaying")
  val Acting=Value("Acting")
  val Waiting=Value("Waiting")
}

object CombatantType extends Enumeration {
  val Minion=Value("Minion")
  val Character=Value("Character")
  val Monster=Value("Monster")
  
  def isCharacter(ctype:this.Value) = ctype==Character
}