import org.scalatest._

import nnsp.P04

class P04Test extends FlatSpec with Matchers {
  "Function length" should "return n for a list of size n" in {
    P04.length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }

  "Function length" should "return 0 for an empty list" in {
    P04.length(List()) should be (0)
  }
}
