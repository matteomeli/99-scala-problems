package nnsp

import P05._
import P09._

object P10 {
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
