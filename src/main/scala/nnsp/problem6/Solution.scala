package nnsp.problem6

import nnsp.problem5.Solution._

object Solution {
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)
}
