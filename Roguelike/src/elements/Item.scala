/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements




sealed abstract class Item extends Element{
  def Use(playerCharacter : PlayerCharacter) = this match
  {
    case equipament : Equipament => equipament.Equip(playerCharacter)
    case consumable : Consumable => consumable.Consume(playerCharacter)
  }
}


case class Equipament(damage : Int) extends Item
{
  def Equip(playerCharacter : PlayerCharacter) : Equipament = 
  {
      var oldEquip : Equipament  = playerCharacter.getEquipament()
      playerCharacter.setEquipament(this)
      return oldEquip
      
  }
  def getDamage() : Int = this.damage
}
case class Consumable(heal : Int) extends Item
{
  def Consume(playerCharacter : PlayerCharacter)
  {
    playerCharacter.Heal(heal)
  }
}


object Item {}