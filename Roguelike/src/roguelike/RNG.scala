/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike

import util.Random

object RNG {
  val rand = new Random(11111111)
  
  def randInt(start : Int, count : Int) : Int =
    if(count == 0) start else rand.nextInt(count) + start
}
