package nnsp.problem5

object Solution {
  def reverse[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[A]): List[A] = l match {
      case head :: tail => loop(tail, head :: result)
      case _ => result
    }
    loop(list, List())
  }
}
