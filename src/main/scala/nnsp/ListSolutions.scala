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
    @annotation.tailrec
    def go(rem: List[A], acc: List[(Int, A)]): List[(Int, A)] = rem match {
      case Nil => reverse(acc)
      case x :: xs if acc.isEmpty || x != acc.head._2 => go(xs, (1, x) :: acc)
      case x :: xs => go(xs, (acc.head._1 + 1, x) :: acc.tail)
    }
    go(ls, List())
  }

  // Problem 14
  def duplicate1[A](ls: List[A]): List[A] = {
    @annotation.tailrec
    def go(rem: List[A], acc: List[A]): List[A] = rem match {
      case Nil => reverse(acc)
      case x :: xs => go(xs, x :: x :: acc)
    }
    go(ls, List())
  }

  def duplicate2[A](ls: List[A]): List[A] = ls flatMap { List.fill(2)(_) }

  def duplicate[A](ls: List[A]): List[A] = duplicate2(ls)

  // Problem 15
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap { List.fill(n)(_) }

  // Problem 16
  def dropEveryN[A](n: Int, ls: List[A]): List[A] = {
    @annotation.tailrec
    def go(i: Int, rem: List[A], acc: List[A]): List[A] = rem match {
      case Nil => reverse(acc)
      case x :: xs if i == 0 => go(n - 1, xs, acc)
      case x :: xs => go(i - 1, xs, x :: acc)
    }
    go(n - 1, ls, List())
  }

  def dropEveryN1[A](n: Int, ls: List[A]): List[A] = ls.zipWithIndex filter { _._2 + 1 % n != 0 } map { _._1 }

  // Problem 17
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def go(i: Int, curr: List[A], prefix: List[A]): (List[A], List[A]) = 
      if (i == 0) (reverse(prefix), curr)  
      else curr match {
        case Nil => (reverse(prefix), List())
        case x :: xs => go(i - 1, xs, x :: prefix)
      }
    go(n, ls, List())
  }

  def split1[A](n: Int, ls: List[A]): (List[A], List[A]) = (ls.take(n), ls.drop(n))

  // Problem 18
  def slice[A](i: Int, k: Int, ls: List[A]): List[A] = if (k <= i) List() else { val s = i max 0; ls.drop(s).take(k - s) }

  // Problem 19
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else if (n < 0) n + ls.length else n % ls.length
    ls.drop(nBounded) ++ ls.take(nBounded)
  }

  // Problem 20
  def removeAt[A](n: Int, ls: List[A]): (List[A], Option[A]) = if (n < 0) (ls, None) else {
    (ls.take(n), ls.drop(n)) match {
      case (p, Nil) => (p, None)
      case (p, x :: xs) => (p ++ xs, Some(x))
    }
  }

  // Problem 21
  def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = {
    val nBounded = if (n < 0) 0 else n min ls.length
    ls.take(nBounded) ++ (a :: ls.drop(nBounded))
  }

  def insertAt1[A](a: A, n: Int, ls: List[A]): List[A] =
    ls.splitAt(n) match { case (pre, post) => pre ::: a :: post }
}
