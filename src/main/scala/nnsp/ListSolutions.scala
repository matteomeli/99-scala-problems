package nnsp

object ListSolutions {
  // Problem 1
  @annotation.tailrec
  def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  @annotation.tailrec
  def lastOption[A](list: List[A]): Option[A] = list match {
    case head :: Nil => Some(head)
    case head :: tail => lastOption(tail)
    case Nil => None
  }

  // Problem 2
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

  // Problem 3
  @annotation.tailrec
  def nth[A](n: Int, list: List[A]): Option[A] = (n, list) match {
    case (0, head :: _) => Some(head)
    case (_, _ :: tail) => nth(n - 1, tail)
    case _ => None
  }

  // Problem 4
  def length[A](list: List[A]): Int = {
    @annotation.tailrec
    def loop(l: List[A], acc: Int): Int = l match {
      case _ :: tail => loop(tail, acc + 1)
      case _ => acc
    }
    loop(list, 0)
  }

  // Problem 5
  def reverse[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[A]): List[A] = l match {
      case head :: tail => loop(tail, head :: result)
      case _ => result
    }
    loop(list, List())
  }

  // Problem 6
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)

  // Problem 7
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

  // Problem 8
  def compress[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[A]): List[A] = l match {
      case first :: second :: tail if first == second => loop(first :: tail, result)
      case head :: tail => loop(tail, head :: result)
      case _ => reverse(result)
    }
    loop(list, List())
  }

  // Problem 9
  def pack[A](list: List[A]): List[List[A]] = {
    @annotation.tailrec
    def loop(l: List[A], result: List[List[A]]): List[List[A]] = (l, result) match {
      case (x :: xs, (h :: t) :: ys) if x == h => loop(xs, (x :: h :: t) :: ys)
      case (y :: ys, l) => loop(ys, List(y) :: l)
      case _ => reverse(result)
    }
    loop(list, List())
  }

  // Problem 10
  def encode[A](list: List[A]): List[(Int, A)] = {
    @annotation.tailrec
    def loop(l: List[List[A]], result: List[(Int, A)]): List[(Int, A)] = l match {
      case x :: xs => loop(xs, (x.length, x.head) :: result)
      case Nil => reverse(result)
    }
    loop(pack(list), List())
  }

  // Problem 11
  def encodeModified[A](ls: List[A]): List[Any] = {
    encode(ls).map { e =>
      if (e._1 == 1) e._2
      else e
    }
  }

  def encodeModified1[A](ls: List[A]): List[Either[A, (Int, A)]] = {
    encode(ls).map { e =>
      if (e._1 == 1) Left(e._2)
      else Right(e)
    }
  }

  // Problem 12
  def decode1[A](ls: List[(Int, A)]): List[A] = {
    def go(rest: List[(Int, A)], acc: List[A]): List[A] = rest match {
      case Nil => reverse(acc)
      case x :: xs if x._1 == 0 => go(xs, acc)
      case x :: xs => go((x._1 - 1, x._2) :: xs, x._2 :: acc)
    }
    go(ls, List())
  }

  def decode2[A](ls: List[(Int, A)]): List[A] = {
    ls flatMap { e => List.fill(e._1)(e._2) }
  }

  def decode[A](ls: List[(Int, A)]): List[A] = decode2(ls)

  // Problem 13
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    def go(rem: List[A], acc: List[(Int, A)]): List[(Int, A)] = rem match {
      case Nil => reverse(acc)
      case x :: xs if acc.isEmpty || x != acc.head._2 => go(xs, (1, x) :: acc)
      case x :: xs => go(xs, (acc.head._1 + 1, x) :: acc.tail)
    }
    go(ls, List())
  }
}
