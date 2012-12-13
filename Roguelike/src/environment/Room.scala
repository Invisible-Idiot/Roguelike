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

sealed abstract class Area {
  protected var playerCharacter : Option[PlayerCharacter]
  
  final def hasPlayer : Boolean = !playerCharacter.isEmpty
  
  def draw(canvas : Array[Array[Char]]) : Unit
  def tick() : Unit
  def move(direction : Direction) : Unit
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) : Unit
  
  def message : String = playerCharacter.map{ pc => pc.dataToString + "\n" + pc.applyEffects() }.getOrElse("")
}

object Area {
  val doorChar = '0'
}

class Room(top : Int, left : Int, height : Int, width : Int) extends Area {
  private val maxMonstersPerRoom = 5
  private val map : Array[Array[Element]] = Array.fill(height, width)(Space)
  protected var playerCharacter : Option[PlayerCharacter] = None
  private var playerPosition : (Int, Int) = (-1, -1)
  private var monsters : Map[(Int, Int), Monster] = populate()
  private var doors : Map[(Int, Int), Tunnel] = Map()
  
  def chooseSpot(direction : Direction) : (Int, Int) = direction match {
    case Left => (RNG.randInt(top, height), left - 1)
    case Right => (RNG.randInt(top, height), left + width)
    case Up => (top - 1, RNG.randInt(left, width))
    case Down => (top + height, RNG.randInt(left, width))
  }
  
  def digTunnel(entrance : (Int, Int), tunnel : Tunnel) = {
    doors += (toLocal(entrance) -> tunnel)
  }
  
  def insertPlayer(pc : PlayerCharacter) = {
    playerCharacter = Some(pc)
    playerPosition = (RNG.randInt(0, height), RNG.randInt(0, width))
  }
  
  def draw(canvas : Array[Array[Char]]) = {
    drawWalls(canvas)
    drawDoors(canvas)
    
    if(!playerCharacter.isEmpty) {
      drawFloor(canvas)
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
  
  def drawDoors(canvas : Array[Array[Char]]) = {
    for((i, j) <- doors.keys) {
      canvas(i + top)(j + left) = Area.doorChar
    }
  }
  
  def drawPlayerCharacter(canvas : Array[Array[Char]]) = {
    playerCharacter.foreach { pc : PlayerCharacter =>
      canvas(playerPosition._1 + top)(playerPosition._2 + left) = pc.toChar
    }
  }
  
  def drawMonsters(canvas : Array[Array[Char]]) = {
    for(((i, j), monster) <- monsters) {
      canvas(i + top)(j + left) = monster.toChar
    }
  }
  
  def populate() : Map[(Int, Int), Monster] = {
    val numberOfMonsters = RNG.randInt(0, maxMonstersPerRoom)
    (0 to numberOfMonsters).map(_ => randomPosition() -> Monster.randomMonster).toMap
  }
  
  private def randomPosition() : (Int, Int) = (RNG.randInt(0, height), RNG.randInt(0, width))
  
  def tick() = {
    for(pc <- playerCharacter) {
      monsters = monsters.map((a : ((Int,Int),Monster)) => (a._2.move(playerPosition, a._1, pc), a._2))
    }
  }
  
  def move(direction : Direction) = {
    playerCharacter.foreach(move(_, direction))
  }
  
  private def move(player : PlayerCharacter, direction : Direction) = {
    val newPlayerPosition = direction.movement(playerPosition._1, playerPosition._2)
    /*
    if((monsters.get(newPlayerPosition) match {
      case Some(monster) => player.attack(monster)
      case None => moveTo(newPlayerPosition, direction)})==true)
        monsters-=newPlayerPosition*/
  }
  
  private def moveTo(newPlayerPosition : (Int, Int), movementDirection : Direction) : Boolean= {
    if(inLimits(newPlayerPosition)) {
      playerPosition = newPlayerPosition
      step(newPlayerPosition)
    }
    else doors.get(newPlayerPosition) match {
      case Some(tunnel) => {
          playerCharacter foreach {
            tunnel.enter(_, toGlobal(newPlayerPosition))
          }
          playerCharacter = None
        }
      case None => {}
    }
    return false
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
  private def toLocal(coordinates : (Int, Int)) = (coordinates._1 - top, coordinates._2 - left)
  
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) = {
    this.playerCharacter = Some(playerCharacter)
    playerPosition = (entrance._1 - top, entrance._2 - left)
  }
}

class Tunnel(start : (Int, Int), end : (Int, Int), val startRoom : Room, endRoom : Room, startsHorizontal : Boolean) extends Area {
  protected var playerCharacter : Option[PlayerCharacter] = None
  private val corner = 
    if(startsHorizontal)
      RNG.randInt(1, end._2 - start._2 - 1)
    else
      RNG.randInt(1, end._1 - start._1 - 1)
  
  private val path : Tape[(Direction, Direction)] = makePath()
  private val floorChar = '#'
  
  def draw(canvas : Array[Array[Char]]) = {
    
    val currentPosition = path.headPosition
    val list = path.toList
    val playerCharOrElse = (default : Char) => (counter : Int) =>
      if (!playerCharacter.isEmpty && counter == currentPosition)
        PlayerCharacter.toChar
      else
        default
      
    def drawLoop(cursor : (Int, Int), counter : Int, list : List[(Direction, Direction)]) : Unit = {
      list match {
        case Nil => throw new Exception("Assertion Failure: Path was empty")
        case degreeOfFreedom :: Nil => canvas(cursor._1)(cursor._2) = playerCharOrElse(Area.doorChar)(counter)
        case degreeOfFreedom :: rest => {
            val default = if (counter == 0) Area.doorChar else floorChar
            canvas(cursor._1)(cursor._2) = playerCharOrElse(default)(counter)
            drawLoop(degreeOfFreedom._2.movement(cursor._1, cursor._2), counter + 1, rest)
        }
      }
    }
    
    drawLoop(start, 0, list)
  }
  
  def tick() : Unit = {
  }
  
  def move(direction : Direction) = {
    if(direction == path.read()._1) {
      val moved = path.moveLeft()
      
      if(!moved) {
        playerCharacter foreach { startRoom.enter(_, direction.movement(start._1, start._2)) }
        playerCharacter = None
      }
    }
    else if(direction == path.read()._2) {
      val moved = path.moveRight()
      
      if(!moved) {
        playerCharacter foreach { endRoom.enter(_, direction.movement(end._1, end._2)) }
        playerCharacter = None
      }
    }
  }
  
  def enter(playerCharacter : PlayerCharacter, entrance : (Int, Int)) = {
    this.playerCharacter = Some(playerCharacter)
    
    if(entrance == start)
      path.moveToBeginning()
    else if(entrance == end)
      path.moveToEnd()
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
    
    for(i <- 0 to cornerLength) {
      if(i == 0)
        list = (firstDirection.inverse, if(cornerLength == 0) firstDirection else secondDirection) :: list
      else if(i < cornerLength)
        list = (secondDirection.inverse, secondDirection) :: list
      else
        list = (secondDirection.inverse, firstDirection) :: list
    }
    
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