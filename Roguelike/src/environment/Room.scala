package environment

import elements._

sealed abstract class Direction {
  def movement =
    this match {
      case Left => (position) => (position._1, position._2 - 1)
      case Right => (position) => (position._1, position._2 + 1)
      case Up => (position) => (position._1 - 1, position._2)
      case Down => (position) => (position._1 + 1, position._2)
    }
}

case class Left extends Direction
case class Right extends Direction
case class Up extends Direction
case class Down extends Direction

class Room(top : Int, left : Int, height : Int, width : Int) {
  val map : Array[Array[Element]] = Array.ofDim(height, width)
  var playerCharacter : Option[PlayerCharacter] = None
  var playerPosition : (Int, Int) = (-1, -1)
  var monsters : Map[(Int, Int), Monster] = populate(height, width)
  val doors : Map[(Int, Int), Tunnel] = generateDoors()
  
  def draw(canvas : Array[Array[Char]]) = {
    drawWalls(canvas)
    
    if(playerCharacter.isSome) {
      drawFloor(canvas)
      drawDoors(canvas)
      drawPlayerCharacter(canvas)
      drawMonsters(canvas)
    }
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
    canvas(playerPosition._1)(playerPosition._2) = playerCharacter.toChar
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
  
  def move(direction : Direction) = {
    val newPlayerPosition = direction.movement(playerPosition)
    
    monsters.get(newPlayerPosition) match {
      case Some(monster) => attack(playerCharacter, monster)
      case None => moveTo(newPlayerPosition)
    }
  }
  
  def moveTo(newPlayerPosition : (Int, Int)) = {
    if(inLimits(newPlayerPosition)) {
      playerPosition = newPlayerPosition
      
      
    }
    else doors.get(newPlayerPosition) match {
      case Some(tunnel) => {
        tunnel.enter(playerCharacter)
        playerCharacter = None
      }
      case None => {}
    }
  }
  
  def inLimits(position : (Int, Int)) : Boolean = {
    0 <= position._1 && position._1 < height && 0 <= position._2 && position._2 < width
  }
  
  def step((i : Int, j : Int)) = {
    map(i)(j) match {
      case trap : Trap => {
        trap.spring(playerCharacter)
        map(i)(j) = trap.deactivate
      }
      case item : Item => playerCharacter.pickUp(item)
      case _ => {}
    }
  }
}

class Tunnel(start : (Int, Int), end : (Int, Int)) {
  
}