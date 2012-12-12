/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements
import roguelike.RNG

sealed abstract class Monster(_damage : Int, _health : Int) {
  private var damage : Int = _damage
  private var health : Int = _health

  def toChar = this match {
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
  
  def move(enemyPosition : (Int,Int), myPosition : (Int,Int)) : (Int,Int) =
    {
      if(enemyPosition._1 == myPosition._1)
        if(enemyPosition._2 > myPosition._2)
          return (0,1)
        else if(enemyPosition._2 == myPosition._2)
          return (0,0)
        else
          return (0,-1)
      else if(enemyPosition._1 > myPosition._1)
          if(enemyPosition._2 > myPosition._2)
            return(1,1)
          else if(enemyPosition._2 == myPosition._2)
            return(1,0)
          else
            return(1,-1)
       else
          if(enemyPosition._2 > myPosition._2)
            return(-1,1)
          else if(enemyPosition._2 == myPosition._2)
            return(-1,0)
          else
            return(-1,-1)
    }
   
  override def toString : String = this match {
    case a : MonsterA => "A"
    case b : MonsterB => "B"
  }
}

object Monster{
  def randomMonster : Monster = RNG.randInt(0,2) match {
    case 0 => new MonsterA()
    case _ => new MonsterB()
  }
}

class MonsterA() extends Monster(5,10) {}
class MonsterB() extends Monster(2,25) {}
