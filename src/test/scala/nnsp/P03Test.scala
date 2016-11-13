import org.scalatest._

import nnsp.P03._

class P03Test extends FlatSpec with Matchers {
  "Function nth" should "return the nth for n and a list with at least n elements" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be (Some(2))
    nth(2, List(1, 1, 2)) should be (Some(2))
  }

  "Function nth" should "return None for n and a list with less than n elements" in {
    nth(2, List(1, 1)) should be (None)
  }

  "Function nth" should "return None for any n and a empty list" in {
    // TODO: This looks like a nice test case for Scala Check
    nth(0, List()) should be (None)
    nth(1, List()) should be (None)
    nth(10000, List()) should be (None)
  }

  "Function nth" should "return None for negative n and any list" in {
    nth(-1, List(1, 1, 2)) should be (None)
    nth(-1, List(1, 1)) should be (None)
    nth(-1, List(1)) should be (None)
    nth(-1, List()) should be (None)
  }
}
