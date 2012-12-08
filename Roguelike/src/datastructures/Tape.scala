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
  
  def moveToBeginning() = {
    right ::= head
    right = left.foldLeft(right)((r, x) => x :: r)
  }
  
  def moveToEnd() = {
    left ::= head
    left = right.foldLeft(left)((l, x) => x :: l)
  }
  
  def read() : T = head
  
  def foreach[B](proc : T => B) : Unit = {
    foreachLeft(proc)
    forHead(proc)
    foreachRight(proc)
  }
  
  def foreachLeft[B](proc : T => B) : Unit = left.reverse.foreach(proc)
  
  def forHead[B](proc : T => B) : Unit = proc(head)
  
  def foreachRight[B](proc : T => B) : Unit = right.foreach(proc)
}

object Tape {
  def fromList[T](list : List[T]) : Option[Tape[T]] = list match {
    case Nil => None
    case h :: t => Some(new Tape(h, t))
  }
}
