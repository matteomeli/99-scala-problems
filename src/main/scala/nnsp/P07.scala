package nnsp

object P07 {
  def flatten(list: List[Any]): List[Any] = list match {
    case (head : List[_]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
    case _ => Nil
  }
}
