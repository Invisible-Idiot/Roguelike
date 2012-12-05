/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

sealed abstract class Element

sealed case class Space {
  @Override
  def toString : String = "."
}
sealed case class Staircase {
  @Override
  def toString : String = "%"
}
sealed case class Trap {
  @Override
  def toString : String = "."
}
sealed case class DeactivatedTrap {
  @Override
  def toString : String = "^"
}
sealed case class Gold {
  @Override
  def toString : String = "*"
}
case class Item
