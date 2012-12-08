/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

sealed abstract class Element {
  def toChar : Char =
    this match {
      case Space => '.'
      case Staircase => '%'
      case Trap() => '.'
      case DeactivatedTrap() => '^'
      case Gold => '*'
    }
}

case object Space extends Element
case object Staircase extends Element
sealed case class Trap() extends Element {
  def spring(playerCharacter : PlayerCharacter) = {}
  def deactivate = DeactivatedTrap()
}
sealed case class DeactivatedTrap() extends Element
case object Gold extends Element
case class Item extends Element
