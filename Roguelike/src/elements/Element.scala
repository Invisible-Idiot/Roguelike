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
      case Trap => '.'
      case DeactivatedTrap => '^'
      case Gold => '*'
    }
}

sealed case object Space
sealed case object Staircase
sealed case class Trap
sealed case class DeactivatedTrap
sealed case object Gold
case class Item
