/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package elements


class PlayerCharacter {
  type Effect = (String, PlayerCharacter => Unit)
  
  def toChar : Char = PlayerCharacter.toChar
  private var effects : List[Effect] = List()
  private var items : List[Item] = List()
  private var health : Int = 100
  private var attackDamage : Int = 5
  private var attackMonster : Monster => Boolean = attackWith(Equipament(0))
  
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
  
  def receiveEffect(effect : Effect) = {
    effects = effect :: effects
  }
  
  def applyEffects() : String = {
    val identityEffect = ("", (you : PlayerCharacter) => {})
    val composeEffect = (effect1 : Effect, effect2 : Effect) =>
      (effect1._1 + " " + effect2._1, (x : PlayerCharacter) => {effect1._2(x); effect2._2(x)})
      
    val composedEffect = effects.foldRight(identityEffect)(composeEffect)
    
    effects = List()
    
    composedEffect._2(this)
    return composedEffect._1
  }
  
  def sufferDamage(damage : Int) : Boolean = // retorna verdadeiro se o personagem morreu
  {
      health = health - damage
      if ( health < 0)
        return true
      else
        return false
  }
  
  def attack(monster : Monster) = attackMonster(monster)
  
  private def attackWith(equipment : Equipament)(monster : Monster) : Boolean = 
  {
    effects = ("You attacked the " + monster.toString + ".", (you : PlayerCharacter) => {}) :: effects
    
    monster.sufferDamage(attackDamage + equipment.getDamage)
  }
  
  def equip(equip : Equipament) =
  {
    this.attackMonster = attackWith(equip)
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
