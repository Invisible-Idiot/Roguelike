/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

class PlayerCharacter {
  def toChar : Char = PlayerCharacter.toChar
  
  def pickUp(item : Item) = {}
  
  def attack(monster : Monster) = {}
}

object PlayerCharacter {
  def toChar : Char = '@'
}
