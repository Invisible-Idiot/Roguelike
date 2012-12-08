/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike

import environment.Dungeon

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    while(true) {
      val drawing = Dungeon.draw()
      val input = Console.in.read.toChar
      Dungeon.update(input)
    }
  }

}
