package nnsp

import scala.annotation.tailrec

object P07 {
  def flatten(list: List[Any]): List[Any] = {
    @tailrec
    def flattenRec(result: List[Any], current: List[Any]): List[Any] = current match {
      case (head : List[_]) :: Nil => flattenRec(result, head)
      case (head : List[_]) :: tail => flattenRec(result ::: head, tail)
      case head :: tail => flattenRec(result ::: List(head), tail)
      case _ => result
    }
    flattenRec(List(), list)
  }
}
