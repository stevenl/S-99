package s_99

import org.scalatest.FreeSpec
import s_99.Lists._

class ListsSuite extends FreeSpec {
  "last()" - {
    "should return last element if multiple elements" in {
      assert(last(List(1, 2, 3, 4, 5)) == 5)
    }
    "should throw when given empty list" in {
      assertThrows[NoSuchElementException] {
        last(List())
      }
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
      assertThrows[NoSuchElementException] {
        penultimate(List())
      }
    }
    "should throw when 1 element" in {
      assertThrows[NoSuchElementException] {
        penultimate(List(1))
      }
    }
  }

  "nth()" - {
    "should return the first element if n is 0" in {
      assert(nth(0, List(1, 2, 3)) == 1)
    }
    "should return the nth element" in {
      assert(nth(2, List(1, 2, 3)) == 3)
    }
    "should throw when n is negative" in {
      assertThrows[NoSuchElementException] {
        nth(-1, List(1))
      }
    }
    "should throw when list is shorter than n - 1" in {
      assertThrows[NoSuchElementException] {
        nth(3, List(1, 2, 3))
      }
    }
  }

  "length()" - {
    "should return 0 for an empty list" in {
      assert(length(List()) == 0)
    }
    "should return the number of elements in the list" in {
      assert(length(List(1, 2, 3)) == 3)
    }
  }

  "reverse()" - {
    "should return an empty list if it is empty" in {
      assert(reverse(List()) == List())
    }
    "should return the list in reverse order" in {
      assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    }
  }

  "isPalindrome()" - {
    "should return true for an empty list" in {
      assert(isPalindrome(List()) == true)
    }
    "palindrome" in {
      assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
    }
    "not a palindrome" in {
      assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
    }
  }

  "flatten()" - {
    assert(
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8)
    )
  }

  "compress()" - {
    assert(
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
        List('a, 'b, 'c, 'a, 'd, 'e)
    )
  }

  "pack()" - {
    assert(
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  "encode()" - {
    assert(
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    )
  }

  "encodeModified()" - {
    assert(
      encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
        List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    )
  }
}
