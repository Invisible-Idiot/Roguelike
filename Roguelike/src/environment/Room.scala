package environment

import elements._

class Room(top : Int, left : Int, height : Int, width : Int) {
  val map : Array[Array[Element]] = Array.ofDim(height, width)
  var playerCharacter : Option[PlayerCharacter] = None
  var playerPosition : (Int, Int) = (-1, -1)
  var monsters : Map[(Int, Int), Monster] = populate(height, width)
  
  def draw(canvas : Array[Array[Char]]) = {
    
  }
  
  def populate(height : Int, width : Int) : Map[(Int, Int), Monster] = {
    
  }
  
  def tick() = {
    
  }
}

class Tunnel(start : (Int, Int), end : (Int, Int)) {
  
}