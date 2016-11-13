package nnsp

import scala.annotation.tailrec

object P05 {
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseRec(result: List[A], current: List[A]): List[A] = current match {
      case head :: tail => reverseRec(head :: result, tail)
      case _ => result
    }
    reverseRec(List(), list)
  }
}
