import org.scalatest._

import nnsp.ListSolutions._

class ListTest extends FlatSpec with Matchers {
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

  "Function length" should "return n for a list of size n" in {
    nnsp.ListSolutions.length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }

  "Function length" should "return 0 for an empty list" in {
    nnsp.ListSolutions.length(List()) should be (0)
  }

  "Function reverse" should "return a reversed list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  "Function reverse" should "return an identical list for a list of one element" in {
    reverse(List(1)) should be (List(1))
  }

  "Function reverse" should "return an empty list for an empty list" in {
    reverse(List()) should be (List())
  }

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

  "Function flatten" should "return a flattened list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }

  "Function flatten" should "return the input list if it's already flattened" in {
    flatten(List(1, 1, 2, 3, 5, 8)) should be (List(1, 1, 2, 3, 5, 8))
  }

  "Function flatten" should "return an empty list for an empty list" in {
    flatten(List()) should be (List())
  }

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

  "Function pack" should "return a list with consecutive elements in sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "Function pack" should "return an empty list for an empty list" in {
    pack(List()) should be (List())
  }

  "Function encode" should "return the run-length encoding of a list" in {
    encode((List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "Function encodeModified" should "return the modified run-length encoding of a list" in {
    encodeModified((List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  "Function encodeModifiedSafe" should "return the modified run-length encoding of a list in a type safe way" in {
    encodeModified1((List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) should be (List(Right(4,'a), Left('b), Right((2,'c)), Right(2,'a), Left('d), Right(4,'e)))
  }

  "Function decode" should "return the uncompressed run-length encoding of a list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "Function encodeDirect" should "return the run-length encoding of a list" in {
    encodeDirect((List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "Function duplicate" should "duplicate each element of a list" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "Function duplicateN" should "duplicate each element of a list an N number of times" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "Function dropEveryN" should "drop every element at nth position of a list" in {
     dropEveryN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "Function split" should "split a list in two at the specified index" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "Function slice" should "split a slice from a list from the specified indexes (final excluded)" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
  }

  "Function rotate" should "rotate a list n places to the left" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "Function removeAt" should "remove element at nth position from a list and return the list and the element in a tuple" in {
    removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd), Some('b)))
    removeAt(4, List('a, 'b, 'c, 'd)) should be ((List('a, 'b, 'c, 'd), None))
  }

  "Function insertAt" should "insert element at nth position in a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
  }
}
