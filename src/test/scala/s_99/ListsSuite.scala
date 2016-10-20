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

  "decode()" - {
    assert(
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ==
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    )
  }

  "encodeDirect()" - {
    assert(
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    )
  }

  "duplicate()" - {
    assert(
      duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    )
  }

  "duplicateN()" - {
    assert(
      duplicateN(3, List('a, 'b, 'c, 'c, 'd)) ==
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    )
  }

  "drop()" - {
    assert(
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  "split()" - {
    assert(
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
        (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }

  "slice()" - {
    assert(
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g)
    )
  }

  "rotate()" - {
    "positive N" in {
      assert(
        rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      )
    }
    "negative N" in {
      assert(
        rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
      )
    }
    "N equal to length" in {
      assert(
        rotate(11, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      )
    }
    "N greater than length" in {
      assert(
        rotate(13, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
          List('c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b)
      )
    }
  }

  "removeAt()" - {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd ), 'b))
  }

  "insertAt()" - {
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }

  "range()" - {
    "Invalid arguments" in {
      assertThrows[IllegalArgumentException] { range(9, 4) }
    }
    "equal arguments" in {
      assert(range(4, 4) == List(4))
    }
    "normal" in {
      assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
    }
  }

  "randomSelect()" - {
    var result = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h ))
    assert(result.length == 3 && result.toSet.subsetOf(Set('a, 'b, 'c, 'd, 'f, 'g, 'h )))
  }

  "lotto()" - {
    var result = lotto(6, 49)
    assert(result.length == 6 && result.forall(_ <= 49))
  }

  "randomPermute()" - {
    var input = List('a, 'b, 'c, 'd, 'e, 'f)
    var result = randomPermute(input)
    assert(result.length == 6 && result != input && result.toSet == input.toSet)
  }

  "combinations()" - {
    assert(
      combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) ==
        List(
          List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), List('a, 'b, 'f),
          List('a, 'c, 'd), List('a, 'c, 'e), List('a, 'c, 'f), List('a, 'd, 'e),
          List('a, 'd, 'f), List('a, 'e, 'f), List('b, 'c, 'd), List('b, 'c, 'e),
          List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f), List('b, 'e, 'f),
          List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f), List('d, 'e, 'f)
        )
    )
  }

  "group()" - {
    "simple" in {
      assert(
        group(List(2, 2), List("Aldo", "Beat", "Carla", "David")) ==
          List(
            List(List("Aldo", "Beat"), List("Carla", "David")),
            List(List("Aldo", "Carla"), List("Beat", "David")),
            List(List("Aldo", "David"), List("Beat", "Carla")),
            List(List("Beat", "Carla"), List("Aldo", "David")),
            List(List("Beat", "David"), List("Aldo", "Carla")),
            List(List("Carla", "David"), List("Aldo", "Beat"))
          )
      )
    }
    "group3" in {
      assert(
        group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) ==
          group(List(2, 3, 4), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
      )
    }
  }
}
