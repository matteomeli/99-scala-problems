package nnsp.problem1

object Solution {
  @annotation.tailrec
  def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  @annotation.tailrec
  def lastOption[A](list: List[A]): Option[A] = list match {
    case head :: Nil => Some(head)
    case head :: tail => lastOption(tail)
    case Nil => None
  }
}
