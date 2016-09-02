package s_99

import org.scalatest.FreeSpec

import s_99.Lists._

class ListsSuite extends FreeSpec {
  "last()" - {
    "should return last element if multiple elements" in {
      assert(last(List(1, 2, 3, 4, 5)) == 5)
    }
    "should throw when given empty list" in {
      assertThrows[NoSuchElementException] { last(List()) }
    }
    "should return first element if only one element" in {
      assert(last(List(5)) == 5)
    }
  }

  "penultimate()" - {
    "should return second last element if multiple elements" in {
      assert(penultimate(List(1, 2, 3, 4, 5)) == 4)
    }
    "should throw when no elements" in {
      assertThrows[NoSuchElementException] { penultimate(List()) }
    }
    "should throw when 1 element" in {
      assertThrows[NoSuchElementException] { penultimate(List(1)) }
    }
  }
}
