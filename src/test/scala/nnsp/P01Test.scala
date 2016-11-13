import org.scalatest._

import nnsp.P01._

class P01Test extends FlatSpec with Matchers {
  "The last function" should "return last element of a non empty list" in {
    last(List(1, 1, 2, 3, 5, 8)) should be (8)
    last(List(1)) should be (1)
  }

  "The last function" should "throw a NoSuchElementException for an empty list" in {
    an [NoSuchElementException] should be thrownBy last(List())
  }

  "The lastOption function" should "return the last element of a non empty list" in {
    lastOption(List(1, 1, 2, 3, 5, 8)) should be (Some(8))
    lastOption(List(1)) should be(Some(1))
  }

  "The lastOption function" should "return None for an empty list" in {
    lastOption(List()) should be(None)
  }
}
