import org.scalatest._

import nnsp.P05._

class P05Test extends FlatSpec with Matchers {
  "Function reverse" should "return a reversed list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  "Function reverse" should "return an identical list for a list of one element" in {
    reverse(List(1)) should be (List(1))
  }

  "Function reverse" should "return an empty list for an empty list" in {
    reverse(List()) should be (List())
  }
}
