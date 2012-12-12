package environment

import roguelike.RNG
import collection.mutable.ArrayBuffer
import elements._

class Dungeon() {
  val height = 20
  val width = 70
  val minRoomHeight = 2
  val minRoomWidth = 9
  val minPartitionHeight = minRoomHeight + 3
  val minPartitionWidth = minRoomWidth + 3
  var areas : List[Area] = List()
  //var rooms : List[Room] = List()
  //var tunnels : List[Tunnel] = List()
  generate()
  
  private def generate() = {
    val hBisection2 = RNG.randInt(minPartitionHeight * 2, height - minPartitionHeight * 3)
    val hBisection1 = RNG.randInt(minPartitionHeight, hBisection2 - minPartitionHeight * 2)
    val vBisection2 = RNG.randInt(minPartitionWidth * 2, width - minPartitionWidth * 3)
    val vBisection1 = RNG.randInt(minPartitionWidth, vBisection2 - minPartitionWidth * 2)
    val partitionMap : Array[Array[Partition]] = Array.ofDim(3, 3)
    
    partitionMap(0)(0) = Partition(0, 0, hBisection1, vBisection1)
    partitionMap(0)(1) = Partition(0, vBisection1, hBisection1, vBisection2 - vBisection1)
    partitionMap(0)(2) = Partition(0, vBisection2, hBisection1, width - vBisection2)
    partitionMap(1)(0) = Partition(hBisection1, 0, hBisection2 - hBisection1, vBisection1)
    partitionMap(1)(1) = Partition(hBisection1, vBisection1, hBisection2 - hBisection1, vBisection2 - vBisection1)
    partitionMap(1)(2) = Partition(hBisection1, vBisection2, hBisection2 - hBisection1, width - vBisection2)
    partitionMap(2)(0) = Partition(hBisection2, 0, height - hBisection2, vBisection1)
    partitionMap(2)(1) = Partition(hBisection2, vBisection1, height - hBisection2, vBisection2 - vBisection1)
    partitionMap(2)(2) = Partition(hBisection2, vBisection2, height - hBisection2, width - vBisection2)
    
    var counter = RNG.randInt(5, 4)
    val roomMap : Array[Array[Option[Room]]] = Array.fill(3, 3)(None)
    val quadrants = new ArrayBuffer[(Int, Int)]()
    quadrants += ((0, 0)); quadrants += ((0, 1)); quadrants += ((0, 2))
    quadrants += ((1, 0)); quadrants += ((1, 1)); quadrants += ((1, 2))
    quadrants += ((2, 0)); quadrants += ((2, 1)); quadrants += ((2, 2))
    
    while(counter > 0) {
      val i = RNG.randInt(0, quadrants.length)
      
      val partition = partitionMap(quadrants(i)._1)(quadrants(i)._2)
      
      roomMap(quadrants(i)._1)(quadrants(i)._2) = Some(buildRoom(partition))
      quadrants.remove(i)
      
      counter -= 1
    }
    
    var connections = List[Connection]()
    
    for(i <- 0 to 2; j <- 0 to 2) roomMap(i)(j) match {
      case Some(room) => {
          areas = room :: areas//rooms = room :: rooms
          
          ((i + 1) to 2) find {
            k : Int => !roomMap(k)(j).isEmpty
          } foreach {
            k : Int => {
              val i1 = i
              val i2 = k
              connections = VerticalConnection(i, k, j) :: connections
            }
          }
          
          ((j + 1) to 2) find {
            k : Int => !roomMap(i)(k).isEmpty
          } foreach {
            k : Int => connections = HorizontalConnection(i, j, k) :: connections
          }
      }
      case None => {}
    }
    
    for(c <- connections) c match {
      case HorizontalConnection(i, j1, j2) => {
          val room1 = roomMap(i)(j1)
          val room2 = roomMap(i)(j2)
          for(r1 <- room1; r2 <- room2) areas = digHorizontalTunnel(r1, r2) :: areas//tunnels = digHorizontalTunnel(r1, r2) :: tunnels
      }
      case VerticalConnection(i1, i2, j) => {
          val room1 = roomMap(i1)(j)
          val room2 = roomMap(i2)(j)
          for(r1 <- room1; r2 <- room2) areas = digVerticalTunnel(r1, r2) :: areas//tunnels = digVerticalTunnel(r1, r2) :: tunnels
      }
    }
    
    insertPlayer()
  }
  
  private def insertPlayer() = {
    val areaIndex = RNG.randInt(0, areas.length)
    
    areas(areaIndex) match {
      case room : Room => room.insertPlayer(new PlayerCharacter())
      case tunnel : Tunnel => tunnel.startRoom.insertPlayer(new PlayerCharacter())
    }
  }
  
  private def buildRoom(partition : Partition) : Room = {
    val top = partition.top + RNG.randInt(1, partition.height - minRoomHeight - 2)
    val left = partition.left + RNG.randInt(1, partition.width - minRoomWidth - 2)
    val height = RNG.randIntBetween(minRoomHeight, partition.top + partition.height - top - 1)
    val width = RNG.randIntBetween(minRoomWidth, partition.left + partition.width - left - 1)
    
    return new Room(top, left, height, width)
  }
  
  private def digHorizontalTunnel(room1 : Room, room2 : Room) : Tunnel = {
    val entrance1 = room1.chooseSpot(Right)
    val entrance2 = room2.chooseSpot(Left)
    val tunnel = new Tunnel(entrance1, entrance2, room1, room2, true)
    room1.digTunnel(entrance1, tunnel)
    room2.digTunnel(entrance2, tunnel)
    return tunnel
  }
  
  private def digVerticalTunnel(room1 : Room, room2 : Room) : Tunnel = {
    val entrance1 = room1.chooseSpot(Down)
    val entrance2 = room2.chooseSpot(Up)
    val tunnel = new Tunnel(entrance1, entrance2, room1, room2, false)
    room1.digTunnel(entrance1, tunnel)
    room2.digTunnel(entrance2, tunnel)
    return tunnel
  }
  
  def draw() : String = {
    val canvas = Array.fill(height, width)(' ')
    
    for(area <- areas) area.draw(canvas)
    
    val drawing = new StringBuilder()
    
    for(i <- 0 until height) {
      for(j <- 0 until width) {
        drawing += canvas(i)(j)
      }
      
      drawing += '\n'
    }
    
    return drawing.mkString
  }
  
  def move(direction : Direction) = {
    forPlayerCharacter(_.move(direction))
  }
  
  def tick() : Unit = {
    areas.map(_.tick())
  }
  
  private def forPlayerCharacter[T](action : Area => T) : T = {
    def lookForPlayerCharacter(list : List[Area])(action : Area => T) : T = list match {
      case Nil => throw new Exception("Assertion Failure: Can't find player")
      case h :: t => if(!h.hasPlayer) lookForPlayerCharacter(t)(action) else action(h)
    }
    
    lookForPlayerCharacter(areas)(action)
  }
}