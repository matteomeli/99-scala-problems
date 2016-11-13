package nnsp

import scala.annotation.tailrec

object P02 {
  @tailrec
  def penultimate[A](list: List[A]): A = list match {
    case lastButOne :: _ :: Nil => lastButOne
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  @tailrec
  def penultimateOption[A](list: List[A]): Option[A] = list match {
    case lastButOne :: _ :: Nil => Some(lastButOne)
    case _ :: tail => penultimateOption(tail)
    case _ => None
  }
}
