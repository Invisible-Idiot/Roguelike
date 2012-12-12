/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements

class PlayerCharacter {
  def toChar : Char = PlayerCharacter.toChar
  private var items : List[Item] = List()
  private var health : Int = 100
  private var attackDamage : Int = 5
  private var equipament : Option[Equipament] = None
  
  def dataToString() : String =
    {
      var retvalue : String = "Items = "
      for(i : Item <- items)
        retvalue = retvalue + i.toString() + ","
      retvalue = retvalue + "; Health = " + health.toString()
      retvalue = retvalue + "; Attack Damage = " + attackDamage.toString()
      retvalue = retvalue + "; Equipaments = "
      for(i : Equipament <- equipament)
        retvalue = retvalue + i.toString() + ","
      return retvalue
    }
  
  def pickUp(item : Item) =  
  {
    items = items.union(List(item))
  }
  
  def sufferDamage(damage : Int) : Boolean = // retorna verdadeiro se o personagem morreu
  {
      health = health - damage
      if ( health < 0)
        return true
      else
        return false
  }
  
  def attack(monster : Monster) : Boolean = 
  {
    if(equipament.isDefined)
      monster.sufferDamage(attackDamage + equipament.get.getDamage)
    else
      monster.sufferDamage(attackDamage) 
  }
  
  def setEquipament(equip : Option[Equipament]) =
  {
    this.equipament = equip
  }
  
  def getEquipament() : Option[Equipament] = 
    {
      if(equipament.isDefined)this.equipament else None
    }
  
  def Heal(heal : Int) =
  {
    health = health + heal
  }
  
  def getItems() : List[Item] = this.items
}

object PlayerCharacter {
  def toChar : Char = '@'
  
}
