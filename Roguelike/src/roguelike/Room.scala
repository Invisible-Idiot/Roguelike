package environment

class Room(top : Int, left : Int, width : Int, height : Int) {
  val map : Array[Array[Element]]
  var playerCharacter : Option[PlayerCharacter]
  var playerPosition : (Int, Int)
  var monsters : Map[(Int, Int), Monster]
  
  def draw(canvas : Array[Array[Char]]) = {
    
  }
}