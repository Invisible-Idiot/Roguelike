package environment

import elements._

class Room(top : Int, left : Int, height : Int, width : Int) {
  val map : Array[Array[Element]] = Array.ofDim(height, width)
  var playerCharacter : Option[PlayerCharacter] = None
  var playerPosition : (Int, Int) = (-1, -1)
  var monsters : Map[(Int, Int), Monster] = populate(height, width)
  
  def draw(canvas : Array[Array[Char]]) = {
    drawWalls(canvas)
    drawFloor(canvas)
    drawDoors(canvas)
    drawPlayerCharacter(canvas)
    drawMonsters(canvas)
  }
  
  def drawWalls(canvas : Array[Array[Char]]) = {
    canvas(top - 1)(left - 1) = '+'
    canvas(top - 1)(left + width) = '+'
    canvas(top + height)(left - 1) = '+'
    canvas(top + height)(left + width) = '+'
    
    for(i <- top until (top + height)) {
      canvas[i][left - 1] = '|'
      canvas[i][left + width] = '|'
    }
    
    for(j <- left until (left + width)) {
      canvas[top - 1][j] = '-'
      canvas[top + height][j] = '-'
    }
  }
  
  def drawFloor(canvas : Array[Array[Char]]) = {
    for(i <- 0 until height; j <- 0 until width) {
      canvas[i + top][j + left] = map[i][j].toChar
    }
  }
  
  def drawDoors(canvas : Array[Array[Char]]) = {
    
  }
  
  def drawPlayerCharacter(canvas : Array[Array[Char]]) = {
    if(playerCharacter.isSome) {
      canvas(playerCharacterPosition._1)(playerCharacterPosition._2) = playerCharacter.toChar
    }
  }
  
  def drawMonsters(canvas : Array[Array[Char]]) = {
    for(((i, j), monster) <- monsters) {
      canvas(i)(j) = monster.toChar
    }
  }
  
  def populate(height : Int, width : Int) : Map[(Int, Int), Monster] = {
    
  }
  
  def tick() = {
    
  }
}

class Tunnel(start : (Int, Int), end : (Int, Int)) {
  
}