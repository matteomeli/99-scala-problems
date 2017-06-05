package nnsp.problem10

import nnsp.problem5.Solution._
import nnsp.problem9.Solution._

object Solution {
  // TODO: Find a solution without using reverse
  def encode[A](list: List[A]): List[(Int, A)] = {
    @annotation.tailrec
    def loop(l: List[List[A]], result: List[(Int, A)]): List[(Int, A)] = l match {
      case x :: xs => loop(xs, (x.length, x.head) :: result)
      case Nil => reverse(result)
    }
    loop(pack(list), List())
  }
}
