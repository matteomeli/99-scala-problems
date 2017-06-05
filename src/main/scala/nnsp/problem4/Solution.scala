package nnsp.problem4

object Solution {
  def length[A](list: List[A]): Int = {
    @annotation.tailrec
    def loop(l: List[A], acc: Int): Int = l match {
      case _ :: tail => loop(tail, acc + 1)
      case _ => acc
    }
    loop(list, 0)
  }
}
