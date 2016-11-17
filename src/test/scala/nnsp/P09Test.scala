import org.scalatest._

import nnsp.P09._

class P09Test extends FlatSpec with Matchers {
  "Function pack" should "return a list with consecutive elements in sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "Function pack" should "return an empty list for an empty list" in {
    pack(List()) should be (List())
  }
}
