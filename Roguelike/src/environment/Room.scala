package environment

import elements._
import datastructures.Tape
import roguelike.RNG

sealed abstract class Direction {
  def movement : (Int, Int) => (Int, Int) =
    this match {
      case Left => (i, j) => (i, j - 1)
      case Right => (i, j) => (i, j + 1)
      case Up => (i, j) => (i - 1, j)
      case Down => (i, j) => (i + 1, j)
    }
  
  def inverse : Direction =
    this match {
      case Left => Right
      case Right => Left
      case Up => Down
      case Down => Up
    }
}

case object Left extends Direction
case object Right extends Direction
case object Up extends Direction
case object Down extends Direction

abstract class Area {
  def hasPlayer : Boolean
  def draw(canvas : Array[Array[Char]]) : Unit
  def tick(direction : Direction) : Unit
  def move(direction : Direction) : Unit
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) : Unit
}

class Room(top : Int, left : Int, height : Int, width : Int) extends Area {
  private val map : Array[Array[Element]] = Array.ofDim(height, width)
  private var playerCharacter : Option[PlayerCharacter] = None
  private var playerPosition : (Int, Int) = (-1, -1)
  private var monsters : Map[(Int, Int), Monster] = populate(height, width)
  private var doors : Map[(Int, Int), (Direction, Tunnel)] = Map()
  
  def chooseSpot(direction : Direction) : (Int, Int) = direction match {
    case Left => (RNG.randInt(top, height), 0)
    case Right => (RNG.randInt(top, height), width - 1)
    case Up => (0, RNG.randInt(left, width))
    case Down => (height - 1, RNG.randInt(left, width))
  }
  
  def digTunnel(entrance : (Int, Int), direction : Direction, tunnel : Tunnel) =
    doors += (entrance -> (direction, tunnel))
  
  def hasPlayer : Boolean = playerCharacter.isEmpty
  
  def draw(canvas : Array[Array[Char]]) = {
    drawWalls(canvas)
    
    if(!playerCharacter.isEmpty) {
      drawFloor(canvas)
      //drawDoors(canvas)
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
      canvas(i)(left - 1) = '|'
      canvas(i)(left + width) = '|'
    }
    
    for(j <- left until (left + width)) {
      canvas(top - 1)(j) = '-'
      canvas(top + height)(j) = '-'
    }
  }
  
  def drawFloor(canvas : Array[Array[Char]]) = {
    for(i <- 0 until height; j <- 0 until width) {
      canvas(i + top)(j + left) = map(i)(j).toChar
    }
  }
  /*
  def drawDoors(canvas : Array[Array[Char]]) = {
    for((i, j) <- doors.keys) {
      canvas(i + top)(j + left) = '0'
    }
  }
  */
  def drawPlayerCharacter(canvas : Array[Array[Char]]) = {
    playerCharacter.foreach { pc : PlayerCharacter =>
      canvas(playerPosition._1)(playerPosition._2) = pc.toChar
    }
  }
  
  def drawMonsters(canvas : Array[Array[Char]]) = {
    for(((i, j), monster) <- monsters) {
      canvas(i)(j) = monster.toChar
    }
  }
  
  def populate(height : Int, width : Int) : Map[(Int, Int), Monster] = {
    Map()
  }
  
  def tick(direction : Direction) = {
    move(direction)
  }
  
  def move(direction : Direction) = {
    val newPlayerPosition = direction.movement(playerPosition._1, playerPosition._2)
    
    monsters.get(newPlayerPosition) match {
      case Some(monster) => playerCharacter.foreach(_.attack(monster))
      case None => moveTo(newPlayerPosition, direction)
    }
  }
  
  private def moveTo(newPlayerPosition : (Int, Int), movementDirection : Direction) = {
    if(inLimits(newPlayerPosition)) {
      playerPosition = newPlayerPosition
      step(newPlayerPosition)
    }
    else doors.get(playerPosition) match {
      case Some((entranceDirection, tunnel)) =>
        if(movementDirection == entranceDirection) {
          playerCharacter foreach {
            tunnel.enter(_, movementDirection.movement(playerPosition._1 + top, playerPosition._2 + left))
          }
          playerCharacter = None
        }
      case None => {}
    }
  }
  
  private def inLimits(position : (Int, Int)) : Boolean = {
    0 <= position._1 && position._1 < height && 0 <= position._2 && position._2 < width
  }
  
  private def step(newPosition : (Int, Int)) = {
    val (i, j) = newPosition
    
    map(i)(j) match {
      case trap : Trap => {
        playerCharacter.foreach(trap.spring(_))
        map(i)(j) = trap.deactivate
      }
      case item : Item => playerCharacter.foreach(_.pickUp(item))
      case _ => {}
    }
  }
  
  private def toGlobal(coordinates : (Int, Int)) = (coordinates._1 + top, coordinates._2 + left)
  
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) = {
    this.playerCharacter = Some(playerCharacter)
    
    doors.get(entrance) match {
      case Some(_) => playerPosition = entrance
      case None => throw new Exception("Assertion Failure: Player entered room from invalid entrance")
    }
  }
}

class Tunnel(start : (Int, Int), end : (Int, Int), startRoom : Room, endRoom : Room, corner : Int, startsHorizontal : Boolean) extends Area {
  private var playerCharacter : Option[PlayerCharacter] = None
  private val path : Tape[(Direction, Direction)] = makePath()
  private val doorChar = '0'
  
  def hasPlayer : Boolean = playerCharacter.isEmpty
  
  def draw(canvas : Array[Array[Char]]) : Unit = {
    var counter = start
    val drawTunnel = (degreeOfFreedom : (Direction, Direction)) => {
      canvas(counter._1)(counter._2) = '#'
      counter = degreeOfFreedom._2.movement(counter._1, counter._2)
    }
    val drawPlayer = (degreeOfFreedom : (Direction, Direction)) => {
      canvas(counter._1)(counter._2) = PlayerCharacter.toChar
      counter = degreeOfFreedom._2.movement(counter._1, counter._2)
    }
    
    path.foreachLeft(drawTunnel)
    if(playerCharacter.isEmpty) path.forHead(drawTunnel) else path.forHead(drawPlayer)
    path.foreachRight(drawTunnel)
    
    canvas(start._1)(start._2) = doorChar
    canvas(end._1)(end._2) = doorChar
  }
  
  def tick(direction : Direction) : Unit = {
    move(direction)
  }
  
  def move(direction : Direction) : Unit = {
    if(direction == path.read()._1) {
      val moved = path.moveLeft()
      
      if(!moved) {
        playerCharacter foreach { startRoom.enter(_, start) }
        playerCharacter = None
      }
    }
    else if(direction == path.read()._2) {
      val moved = path.moveRight()
      
      if(!moved) {
        playerCharacter foreach { startRoom.enter(_, end) }
        playerCharacter = None
      }
      
    }
  }
  
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) = {
    this.playerCharacter = Some(playerCharacter)
    
    if(entrance == start)
      path.moveToBeginning
    else if(entrance == end)
      path.moveToEnd
    else
      throw new Exception("Assertion Failure: Player entered tunnel from invalid entrance")
  }
    
  private def makePath() : Tape[(Direction, Direction)] = {
    val displacement : (Int, Int) = calculateDisplacement(start, end)
    val (firstDirection, secondDirection) = calculateDirections(displacement, startsHorizontal)
    val cornerLength = (if (startsHorizontal) displacement._1 else displacement._2).abs
    val rest = (if (startsHorizontal) displacement._2 else displacement._1) - corner
    
    var list : List[(Direction, Direction)] = List()
    
    for(i <- 0 until corner) list = (firstDirection.inverse, firstDirection) :: list
    list = (firstDirection, secondDirection) :: list
    for(i <- 0 to (cornerLength - 2)) list = (secondDirection.inverse, secondDirection) :: list
    list = (secondDirection.inverse, firstDirection) :: list
    for(i <- 0 until rest) list = (firstDirection.inverse, firstDirection) :: list
    
    return list.reverse match {
      case h :: t => new Tape(h, t)
      case Nil => throw new Exception("Assertion Failure: Tape is empty")
    }
  }
  
  private def calculateDisplacement(start : (Int, Int), end : (Int, Int)) : (Int, Int) =
    (end._1 - start._1, end._2 - start._2)
  
  private def calculateDirections(displacement : (Int, Int), startsHorizontal : Boolean) : (Direction, Direction) = {
    val (i, j) = displacement
    val verticalDir = if(i < 0) Up else Down
    val horizontalDir = if(j < 0) Left else Right
    
    return if (startsHorizontal) (horizontalDir, verticalDir) else (verticalDir, horizontalDir)
  }
}