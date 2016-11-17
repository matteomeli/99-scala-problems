package nnsp

import P05._
import P09._

object P10 {
  def encode[A](list: List[A]): List[(Int, A)] = {
    def loop(l: List[List[A]], result: List[(Int, A)]): List[(Int, A)] = l match {
      case Nil => reverse(result)
      case x :: xs => loop(xs, (x.length, x.head) :: result)
    }
    loop(pack(list), List())
  }
}
