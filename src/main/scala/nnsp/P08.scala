package nnsp

import scala.annotation.tailrec

import P05._

object P08 {
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def compressRec(result: List[A], rest: List[A]): List[A] = rest match {
      case first :: second :: tail if first == second => compressRec(result, first :: tail)
      case head :: tail => compressRec(head :: result, tail)
      case _ => result
    }
    reverse(compressRec(List(), list))
  }
}
