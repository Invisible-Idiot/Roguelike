/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements
import roguelike.RNG
import environment._

sealed abstract class Monster(_damage : Int, _health : Int) {
  private var damage : Int = _damage
  private var health : Int = _health

  def toChar = this match {
      case a : MonsterA => 'A'
      case b : MonsterB => 'B'
  }
  
  def attack : (String, PlayerCharacter => Unit) = 
  {
    ("The " + this.toString + " attacked you.", victim => victim.sufferDamage(damage))   
  }

  def sufferDamage(damage : Int) : Boolean = // retorna true se o monstro morreu
  {
    health = health - damage
    if (health < 0)
      return true
    else
      return false
  }
  /*
  def move(enemyPosition : (Int,Int), myPosition : (Int,Int), playerCharacter : PlayerCharacter) : (Int,Int) =
    {
      if(
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._1 ||
        myPosition._1 == enemyPosition._1 && myPosition._2 == enemyPosition._2 + 1 ||
        myPosition._1 == enemyPosition._1 && myPosition._2 == enemyPosition._2 - 1 ||
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._1 + 1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._1 + 1 ||
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._2 - 1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._2 - 1)
      {
        playerCharacter.receiveEffect(this.attack)
        return (i, j) => (i, j)
      }

      var movement : (Int, Int) => (Int, Int) = Down.movement
      if(enemyPosition._1 == myPosition._1)
        if(enemyPosition._2 > myPosition._2)
          movement = Down.movement
        else if(enemyPosition._2 == myPosition._2)
          movement = Right.movement
        else
          movement = Up.movement
      else if(enemyPosition._1 > myPosition._1)
          if(enemyPosition._2 > myPosition._2)
            movement = position => Down.movement(Right.movement())
          else if(enemyPosition._2 == myPosition._2)
            movement = (1,0)
          else
            movement = (1,-1)
       else
          if(enemyPosition._2 > myPosition._2)
            movement = (-1,1)
          else if(enemyPosition._2 == myPosition._2)
            movement = (-1,0)
          else
            movement = (-1,-1)
          
        //println(retvalue)
        //println(enemyPosition)
        //println((myPosition._1 + retvalue._1,myPosition._2 + retvalue._2))
        return movement(myPosition._1, myPosition._2)
    }*/
  
  def move(enemyPosition : (Int,Int), myPosition : (Int,Int), playerCharacter : PlayerCharacter) : (Int,Int) =
    {
      var retvalue : (Int,Int) = (0,0)
      if(
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._1 ||
        myPosition._1 == enemyPosition._1 && myPosition._2 == enemyPosition._2 + 1 ||
        myPosition._1 == enemyPosition._1 && myPosition._2 == enemyPosition._2 - 1 ||
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._1 + 1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._1 + 1 ||
        myPosition._1 == enemyPosition._1 + 1 && myPosition._2 == enemyPosition._2 - 1 ||
        myPosition._1 == enemyPosition._1 - 1 && myPosition._2 == enemyPosition._2 - 1)
      {
        playerCharacter.receiveEffect(this.attack)
        return(myPosition)
      }

      retvalue = (0,1)
      if(enemyPosition._1 == myPosition._1)
        if(enemyPosition._2 > myPosition._2)
          retvalue = (0,1)
        else if(enemyPosition._2 == myPosition._2)
          retvalue = (0,0)
        else
          retvalue = (0,-1)
      else if(enemyPosition._1 > myPosition._1)
          if(enemyPosition._2 > myPosition._2)
            retvalue = (1,1)
          else if(enemyPosition._2 == myPosition._2)
            retvalue = (1,0)
          else
            retvalue = (1,-1)
       else
          if(enemyPosition._2 > myPosition._2)
            retvalue = (-1,1)
          else if(enemyPosition._2 == myPosition._2)
            retvalue = (-1,0)
          else
            retvalue = (-1,-1)
          
        //println(retvalue)
        //println(enemyPosition)
        //println((myPosition._1 + retvalue._1,myPosition._2 + retvalue._2))
        return (myPosition._1 + retvalue._1,myPosition._2 + retvalue._2)
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
