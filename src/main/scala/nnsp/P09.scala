package nnsp

import P05._

object P09 {
  def pack[A](list: List[A]): List[List[A]] = {
    @annotation.tailrec
    def loop(result: List[List[A]], current: List[A], rest: List[A]): List[List[A]] = (current, rest) match {
      case (Nil, Nil) => result
      case (_ :: _, Nil) => current :: result
      case (Nil, y :: ys) => loop(result, List(y), ys)
      case (x :: _, y :: ys) if x == y => loop(result, y :: current, ys)
      case (x :: _, y :: ys) => loop(current :: result, List(y), ys)
    }
    reverse(loop(List(), List(), list))
  }
}
