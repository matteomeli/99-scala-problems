import org.scalatest._

import nnsp.P08._

class P08Test extends FlatSpec with Matchers {
  "Function compress" should "return a list without consecutive duplicates" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "Function compress" should "return an empty list for an empty list" in {
    compress(List()) should be (List())
  }

  "Function compress" should "return a list of one element for a list of something repeated any times" in {
    compress(List(1, 1, 1, 1, 1, 1, 1)) should be (List(1))
  }

  "Function compress" should "work for nested list" in {
    compress(List(List(1, 1), List(1, 1), 2, 3, 5, 8)) should be (List(List(1, 1), 2, 3, 5, 8))
  }
}
