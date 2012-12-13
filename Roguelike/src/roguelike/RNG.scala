/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package roguelike

import util.Random

object RNG {
  private val rand = new Random(6666666)//11111111)//)//
  
  def randInt(start : Int, range : Int) : Int =
    if(range == 0) start else rand.nextInt(range) + start
  
  def randIntBetween(start : Int, excluded : Int) : Int = randInt(start, excluded - start)
}
