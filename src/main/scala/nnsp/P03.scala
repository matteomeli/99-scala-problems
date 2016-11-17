package nnsp

object P03 {
  @annotation.tailrec
  def nth[A](n: Int, list: List[A]): Option[A] = (n, list) match {
    case (0, head :: _) => Some(head)
    case (_, _ :: tail) => nth(n - 1, tail)
    case _ => None
  }
}
