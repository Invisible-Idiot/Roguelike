package environment

import roguelike.RNG
import collection.mutable.ArrayBuffer

object Dungeon {
  val height = 70
  val width = 70
  var rooms : List[Room] = List()
  var tunnels : List[Tunnel] = List()
  generate()
  
  private def generate() = {
    val hBisection2 = RNG.randInt(6, height - 3)
    val hBisection1 = RNG.randInt(3, hBisection2 - 3)
    val vBisection2 = RNG.randInt(6, width - 3)
    val vBisection1 = RNG.randInt(3, vBisection2 - 3)
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
          rooms = room :: rooms
          
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
          for(r1 <- room1; r2 <- room2) tunnels = digHorizontalTunnel(r1, r2) :: tunnels
      }
      case VerticalConnection(i1, i2, j) => {
          val room1 = roomMap(i1)(j)
          val room2 = roomMap(i2)(j)
          for(r1 <- room1; r2 <- room2) tunnels = digVerticalTunnel(r1, r2) :: tunnels
      }
    }
  }
  
  private def buildRoom(partition : Partition) : Room = {
    val top = RNG.randInt(partition.top + 1, partition.height - 3)
    val left = RNG.randInt(partition.left + 1, partition.width - 3)
    val height = RNG.randInt(1, partition.top + partition.height - top - 2)
    val width = RNG.randInt(1, partition.left + partition.width - left - 2)
    
    return new Room(top, left, height, width)
  }
  
  private def digHorizontalTunnel(room1 : Room, room2 : Room) : Tunnel = {
    val entrance1 = room1.chooseSpot(Right)
    val entrance2 = room2.chooseSpot(Left)
    val corner = RNG.randInt(1, entrance2._2 - entrance1._2)
    val tunnel = new Tunnel(entrance1, entrance2, room1, room2, corner, true)
    room1.digTunnel(entrance1, Right, tunnel)
    room2.digTunnel(entrance2, Left, tunnel)
    return tunnel
  }
  
  private def digVerticalTunnel(room1 : Room, room2 : Room) : Tunnel = {
    val entrance1 = room1.chooseSpot(Down)
    val entrance2 = room2.chooseSpot(Up)
    val corner = RNG.randInt(1, entrance2._1 - entrance1._1)
    val tunnel = new Tunnel(entrance1, entrance2, room1, room2, corner, false)
    room1.digTunnel(entrance1, Down, tunnel)
    room2.digTunnel(entrance2, Up, tunnel)
    return tunnel
  }
  
  def draw() : String = {
    val canvas = Array.fill(height, width)(' ')
    
    for(room <- rooms) room.draw(canvas)
    for(tunnel <- tunnels) tunnel.draw(canvas)
    
    val drawing = new StringBuilder()
    
    for(i <- 0 to height) {
      for(j <- 0 to width) {
        drawing += canvas(i)(j)
      }
      
      drawing += '\n'
    }
    
    return drawing.mkString
  }
  
  def update(input : Char) = input match {
    case 'w' => move(Up)
    case 'a' => move(Left)
    case 's' => move(Down)
    case 'd' => move(Right)
    case _ => {}
  }
  
  private def move(direction : Direction) = forPlayerCharacter(_.move(direction))
  
  private def forPlayerCharacter[T](action : Area => T) = {
    def lookInTunnels(list : List[Tunnel])(action : Area => T) : Unit = list match {
      case Nil => {}
      case h :: t => if(!h.hasPlayer) lookInTunnels(t)(action) else action(h)
    }
    
    def lookInRooms(list : List[Room])(action : Area => T) : Unit = list match {
      case Nil => lookInTunnels(tunnels)(action)
      case h :: t => if(!h.hasPlayer) lookInRooms(t)(action) else action(h)
    }
    
    lookInRooms(rooms)(action)
  }
}