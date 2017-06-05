package nnsp.problem3

object Solution {
  @annotation.tailrec
  def nth[A](n: Int, list: List[A]): Option[A] = (n, list) match {
    case (0, head :: _) => Some(head)
    case (_, _ :: tail) => nth(n - 1, tail)
    case _ => None
  }
}
