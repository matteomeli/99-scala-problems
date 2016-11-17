package nnsp

import P05._

object P09 {
  def pack[A](list: List[A]): List[List[A]] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[List[A]]): List[List[A]] = (l, result) match {
      case (x :: xs, (h :: t) :: ys) if x == h => loop(xs, (x :: h :: t) :: ys)
      case (y :: ys, l) => loop(ys, List(y) :: l)
      case _ => reverse(result)
    }
    loop(list, List())
  }
}
