package nnsp

import P05._

object P06 {
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)
}
