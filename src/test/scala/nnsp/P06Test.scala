import org.scalatest._

import nnsp.P06._

class P06Test extends FlatSpec with Matchers {
  "Function isPalindrome" should "return true if a list is palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
  }

  "Function isPalindrome" should "return false if a list is not palindrome" in {
    isPalindrome(List(1, 2, 3)) should be (false)
  }

  "Function isPalindrome" should "return true for a list of only one element" in {
    isPalindrome(List(1)) should be (true)
  }

  "Function isPalindrome" should "return true for an empty list" in {
    isPalindrome(List()) should be (true)
  }
}
