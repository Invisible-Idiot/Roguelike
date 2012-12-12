/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    while(true) {
      println(Game.draw())
      val input = Console.in.read.toChar//System.console().reader().read.toChar//
      Game.update(input)
    }
  }

}
