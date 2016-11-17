package nnsp

object P02 {
  @annotation.tailrec
  def penultimate[A](list: List[A]): A = list match {
    case lastButOne :: _ :: Nil => lastButOne
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  @annotation.tailrec
  def penultimateOption[A](list: List[A]): Option[A] = list match {
    case lastButOne :: _ :: Nil => Some(lastButOne)
    case _ :: tail => penultimateOption(tail)
    case _ => None
  }
}
