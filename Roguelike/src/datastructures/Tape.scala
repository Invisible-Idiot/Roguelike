/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package datastructures

class Tape[T] (private val hd : T, private val tl : List[T]) {
  var left : List[T] = List()
  var head : T = hd
  var right : List[T] = tl
  
  def moveLeft() : Boolean = {
    left match {
      case Nil => false
      case h :: t => {
          left = t
          right = head :: right
          head = h
          true
      }
    }
  }
  
  def moveRight() : Boolean = {
    right match {
      case Nil => false
      case h :: t => {
          right = t
          left = head :: left
          head = h
          true
      }
    }
  }
  
  def moveToBeginning() : Unit = {
    def move() : Unit =
      left match {
        case Nil => {}
        case h :: Nil => {
            left = Nil
            head = h
        }
        case h :: rest => {
            right ::= h
            left = rest
            move()
        }
      }
    
    if(!left.isEmpty) {
      right ::= head
      move()
    }
  }
  
  def moveToEnd() : Unit = {
    def move() : Unit =
      right match {
        case Nil => {}
        case h :: Nil => {
            right = Nil
            head = h
        }
        case h :: rest => {
            left ::= h
            right = rest
            move()
        }
      }
    
    if(!right.isEmpty) {
      left ::= head
      move()
    }
  }
  
  def read() : T = head
  
  def headPosition : Int = left.length
  
  def toList : List[T] = left.reverse ++ (head :: right)
}

object Tape {
  def fromList[T](list : List[T]) : Option[Tape[T]] = list match {
    case Nil => None
    case h :: t => Some(new Tape(h, t))
  }
}
