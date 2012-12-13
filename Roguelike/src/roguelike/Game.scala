/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike

import environment._

object Game {
  val dungeon = new Dungeon()
  
  def draw() : String = {
    dungeon.draw()
  }
  
  def update(input : Char) = {
    input match {
      case 'w' => dungeon.move(Up)
      case 'a' => dungeon.move(Left)
      case 's' => dungeon.move(Down)
      case 'd' => dungeon.move(Right)
      case _ => {}
    }
    
    dungeon.tick()
  }
}
