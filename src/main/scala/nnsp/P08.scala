package nnsp

import P05._

object P08 {
  def compress[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[A]): List[A] = l match {
      case first :: second :: tail if first == second => loop(first :: tail, result)
      case head :: tail => loop(tail, head :: result)
      case _ => reverse(result)
    }
    loop(list, List())
  }
}
