/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

class PlayerCharacter {
  def toChar : Char = PlayerCharacter.toChar
  var items : Set[Item] = Set()
  var health : Int = 100
  var attackDamage : Int = 0
  var equipament : Equipament
  def pickUp(item : Item) =  
  {
    items = items.+(item)
  }
  
  def sufferDamage(damage : Int) : Boolean = // retorna verdadeiro se o personagem morreu
  {
      health = health - damage
      if ( health < 0)
        return true
      else
        return false
  }
  
  def attack(monster : Monster) = 
  {
    monster.sufferDamage(attackDamage + equipament.getDamage())
  }
  
  def setEquipament(equip : Equipament) =
  {
    this.equipament = equip
  }
  
  def getEquipament() : Equipament = this.equipament
  
  def Heal(heal : Int) =
  {
    health = health + heal
  }
}

object PlayerCharacter {
  def toChar : Char = '@'
  
}
