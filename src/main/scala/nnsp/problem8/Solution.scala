package nnsp.problem8

import nnsp.problem5.Solution._

object Solution {
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
