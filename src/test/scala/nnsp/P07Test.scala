import org.scalatest._

import nnsp.P07._

class P07Test extends FlatSpec with Matchers {
  "Function flatten" should "return a flattened list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }

  "Function flatten" should "return the input list if it's already flattened" in {
    flatten(List(1, 1, 2, 3, 5, 8)) should be (List(1, 1, 2, 3, 5, 8))
  }

  "Function flatten" should "return an empty list for an empty list" in {
    flatten(List()) should be (List())
  }
}
