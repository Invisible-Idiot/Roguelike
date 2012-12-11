/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

sealed abstract class Monster(damage : Int, health : Int) {
  def toChar = this match{
    case MonsterA => 'A'
    case MonsterB => 'B'
  }
  
  def attack(victim : PlayerCharacter)
  {
    victim.sufferDamage(damage)    
  }


  def sufferDamage(damage : Int) : Boolean = // retorna true se o monstro morreu
  {
    health = health - damage
    if (health < 0)
      return true
    else
      return false
  }
}


case class MonsterA extends Monster
case class MonsterB extends Monster
