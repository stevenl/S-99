package s_99

object Lists {
  // P01 (*) Find the last element of a list
  def last[A](list: List[A]): A = {
    def recursive[A](list: List[A]): A = list match {
      case head :: Nil => head
      case _ :: tail => recursive(tail)
      case Nil => throw new NoSuchElementException
    }

    def builtin[A](list: List[A]): A = list.last

    recursive(list)
  }

  // P02 (*) Find the last but one element of a list
  def penultimate[A](list: List[A]): A = {
    def recursive[A](list: List[A]): A = list match {
      case head :: last :: Nil => head
      case head :: tail => recursive(tail)
      case Nil | _ :: Nil => throw new NoSuchElementException
    }

    def builtin1[A](list: List[A]): A = {
      if (list.length > 1) list(list.length - 2)
      else throw new NoSuchElementException
    }

    def builtin2[A](list: List[A]): A = {
      if (!list.isEmpty) list.init.last
      else throw new NoSuchElementException
    }

    recursive(list)
  }

  // P03 (*) Find the Kth element of a list.
  def nth[A](n: Int, list: List[A]): A = {
    def recursive[A](n: Int, list: List[A]): A = {
      if (list.isEmpty || n < 0) throw new NoSuchElementException
      else if (n == 0) list.head
      else recursive(n - 1, list.tail)
    }

    def builtin[A](n: Int, list: List[A]): A = {
      if (0 <= n && n < list.length) list(n)
      else throw new NoSuchElementException
    }

    recursive(n, list)
  }

  // P04 (*) Find the number of elements of a list.
  def length[A](list: List[A]): Int = {
    def recursive[A](list: List[A]): Int = list match {
      case Nil => 0
      case _ :: tail => 1 + recursive(tail)
    }

    def builtin[A](list: List[A]): Int = list.length

    recursive(list)
  }

  // P05 (*) Reverse a list.
  def reverse[A](list: List[A]): List[A] = {
    def recursive[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case head :: tail => recursive(tail) ::: List(head)
    }

    def builtin[A](list: List[A]): List[A] = list.reverse

    recursive(list)
  }

  // P06 (*) Find out whether a list is a palindrome.
  def isPalindrome[A](list: List[A]): Boolean = {
    def a[A](list: List[A]): Boolean = list == reverse(list)
    def b[A](list: List[A]): Boolean = list == list.reverse

    a(list)
  }

  // P07 (**) Flatten a nested list structure.
  def flatten(list: List[Any]): List[Any] = {
    def recursive(list: List[Any]): List[Any] = list match {
      case (head: List[_]) :: tail => recursive(head) ::: recursive(tail)
      case head :: tail => head :: recursive(tail)
      case Nil => Nil
    }

    def flatmap(list: List[Any]): List[Any] = list flatMap {
      case nested: List[_] => flatmap(nested)
      case elem => List(elem)
    }

    flatmap(list)
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  def compress[A](list: List[A]): List[A] = {
    def recursiveCompress[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case x :: xs => {
        if (xs.isEmpty)
          List(x)
        else if (x == xs.head)
          recursiveCompress(xs)
        else
          x :: recursiveCompress(xs)
      }
    }
    //recursiveCompress(list)

    def foldCompress[A](list: List[A]): List[A] = list.foldRight(List[A]()) {
      (x, z) => {
        if (z.isEmpty || x != z.head)
          x :: z
        else
          z
      }
    }
    foldCompress(list)
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  def pack[A](list: List[A]): List[List[A]] = {
    def accPack[A](list: List[A], acc: List[A]): List[List[A]] = list match {
      case Nil => Nil
      case x :: xs => {
        if (xs.isEmpty)
          List(x :: acc)
        else if (x == xs.head)
          accPack(xs, x :: acc)
        else
          List(x :: acc) ::: accPack(xs, Nil)
      }
    }
    //accPack(list, Nil)

    def splitPack[A](list: List[A]): List[List[A]] = {
      def findSplit[A](list: List[A]): Int = list match {
        case Nil => 0
        case x :: xs => {
          if (xs.isEmpty || xs.head != x)
            1
          else
            1 + findSplit(xs)
        }
      }

      val (packed, tail) = list.splitAt(findSplit(list))
      if (tail.isEmpty)
        packed :: Nil
      else
        packed :: splitPack(tail)
    }
    splitPack(list)
  }

  // P10 (*) Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] = {
    for (p <- pack(list)) yield (p.length, p.head)
  }
}
