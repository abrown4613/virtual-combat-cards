//$Id$
package vcc.model

object InitiativeState extends Enumeration {
  val Reserve=Value("Reserve")
  val Ready=Value("Ready")
  val Surprised=Value("Surprised")
  val Delaying=Value("Delaying")
  val Acting=Value("Acting")
  val Waiting=Value("Waiting")
}

object CombatantType extends Enumeration {
  val Minion=Value("Minion")
  val Character=Value("Character")
  val Monster=Value("Monster")
}