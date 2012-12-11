/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

sealed abstract class Monster(_damage : Int, _health : Int) {
  var damage : Int = _damage
  var health : Int = _health

      def toChar = this match{
      case a : MonsterA => 'A'
      case b : MonsterB => 'B'
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

object Monster{

}

case class MonsterA() extends Monster(5,10) {}
case class MonsterB() extends Monster(2,25) {}
