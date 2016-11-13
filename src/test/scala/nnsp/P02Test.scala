import org.scalatest._

import nnsp.P02._

class P02Test extends FlatSpec with Matchers {
  "The penultimate function" should "return the penultimate element of a list with at least two elements" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
    penultimate(List(1, 2)) should be (1)
  }

  "The penultimate function" should "throw a NoSuchElementException for a list with only one element" in {
    a [NoSuchElementException] should be thrownBy penultimate(List(1))
  }

  "The penultimate function" should "throw a NoSuchElementException for an empty list" in {
    a [NoSuchElementException] should be thrownBy penultimate(List())
  }

  "The penultimateOption function" should "return the penultimate element of a list with at least two elements" in {
    penultimateOption(List(1, 1, 2, 3, 5, 8)) should be (Some(5))
    penultimateOption(List(1, 2)) should be (Some(1))
  }

  "The penultimateOption function" should "return None for a list with only one element" in {
    penultimateOption(List(1)) should be (None)
  }

  "The penultimateOption function" should "return None for an empty list" in {
    penultimateOption(List()) should be (None)
  }
}
