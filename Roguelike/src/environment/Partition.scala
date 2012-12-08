/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package environment

case class Partition (top : Int, left : Int, height : Int, width : Int)

sealed abstract class Connection
case class HorizontalConnection(i : Int, j1 : Int, j2 : Int) extends Connection
case class VerticalConnection(i1 : Int, i2 : Int, j : Int) extends Connection
