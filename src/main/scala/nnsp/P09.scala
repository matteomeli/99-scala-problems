package nnsp

import P05._

object P09 {
  def pack[A](list: List[A]): List[List[A]] = {
    @annotation.tailrec
    def loop(result: List[List[A]], rest: List[A]): List[List[A]] = (result, rest) match {
      case ((h :: t) :: xs, y :: ys) if h == y => loop((h :: y :: t) :: xs, ys)
      case (l, y :: ys) => loop(List(y) :: l, ys)
      case _ => result
    }
    reverse(loop(List(), list))
  }
}
