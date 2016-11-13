package nnsp

import scala.annotation.tailrec

object P04 {
  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthRec(acc: Int, current: List[A]): Int = current match {
      case _ :: tail => lengthRec(acc + 1, tail)
      case _ => acc
    }
    lengthRec(0, list)
  }
}
