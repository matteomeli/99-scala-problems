package nnsp.problem9

import nnsp.problem5.Solution._

object Solution {
  // TODO: Find a solution without using reverse
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
