package nnsp

object P05 {
  def reverse[A](list: List[A]): List[A] = {
    def reverseRec(result: List[A], current: List[A]): List[A] = current match {
      case head :: tail => reverseRec(head :: result, tail)
      case _ => result
    }
    reverseRec(List(), list)
  }
}
