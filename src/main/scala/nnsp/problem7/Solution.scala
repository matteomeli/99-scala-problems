package nnsp.problem7

object Solution {
  def flatten(list: List[Any]): List[Any] = {
    @annotation.tailrec
    def loop(l: List[Any], result: List[Any]): List[Any] = l match {
      case (head : List[_]) :: Nil => loop(head, result)
      case (head : List[_]) :: tail => loop(tail, result ::: head)
      case head :: tail => loop(tail, result ::: List(head))
      case _ => result
    }
    loop(list, List())
  }
}
