package s_99

import scala.util.Random

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

  // P11 (*) Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[Any] = {
    for (p <- pack(list)) yield {
       if (p.length > 1)
         (p.length, p.head)
       else
         p.head
    }
  }

  // P12 (**) Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]): List[A] = {
    def repeat(n: Int, x: A): List[A] = (for (i <- 1 to n) yield x).toList
    list flatMap { x => repeat(x._1, x._2) }
  }

  // P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    list.foldRight(List[(Int, A)]()) {
      (x, z) => z match {
        case Nil =>
          (1, x) :: Nil
        case head :: tail =>
          if (x == head._2)
            (head._1 + 1, x) :: tail
          else
            (1, x) :: z
      }
    }
  }

  // P14 (*) Duplicate the elements of a list.
  def duplicate[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  // P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateN[A](n: Int, list: List[A]): List[A] =
    list flatMap { for (i <- 1 to n) yield _ }

  // P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A]): List[A] = {
    def dropi(i: Int, list: List[A]): List[A] = list match {
      case Nil => Nil
      case x :: xs => {
        if (i > 1)
          x :: dropi(i - 1, xs)
        else
          dropi(n, xs)
      }
    }
    dropi(n, list)
  }

  // P17 (*) Split a list into two parts.
  def split[A](i: Int, list: List[A]): (List[A], List[A]) = list match {
    case Nil =>
      (Nil, Nil)
    case x :: xs => {
      if (i == 0)
        (Nil, list)
      else {
        var (l1, l2) = split(i - 1, xs)
        (x :: l1, l2)
      }
    }
  }

  // P18 (**) Extract a slice from a list.
  def slice[A](i: Int, k: Int, list: List[A]): List[A] = {
    if (i > 0)
      slice(i - 1, k - 1, list.tail)
    else if (k > 0)
      list.head :: slice(i, k - 1, list.tail)
    else
      Nil
  }

  // P19 (**) Rotate a list N places to the left.
  def rotate[A](n: Int, list: List[A]): List[A] = {
    if (n < 0)
      rotate(n + list.length, list)
    else if (n >= list.length)
      rotate(n - list.length, list)
    else {
      var (left, right) = split(n, list)
      right ::: left
    }
  }

  // P20 (*) Remove the Kth element from a list.
  def removeAt[A](i: Int, list: List[A]): (List[A], A) = {
    if (i < 0 || list.isEmpty)
      throw new NoSuchElementException
    if (i == 0)
      (list.tail, list.head)
    else {
      var (ls, x) = removeAt(i - 1, list.tail)
      (list.head :: ls, x)
    }
  }

  // P21 (*) Insert an element at a given position into a list.
  def insertAt[A](x: A, i: Int, list: List[A]): List[A] = {
    if (i < 0 || list.isEmpty)
      throw new NoSuchElementException
    if (i == 0)
      x :: list
    else
      list.head :: insertAt(x, i - 1, list.tail)
  }

  // P22 (*) Create a list containing all integers within a given range.
  def range(start: Int, end: Int): List[Int] = {
    if (start > end)
      throw new IllegalArgumentException
    (for (i <- start to end) yield i).toList
  }

  // P23 (**) Extract a given number of randomly selected elements from a list.
  def randomSelect[A](i: Int, list: List[A]): List[A] = {
    if (i < 0)
      throw new IllegalArgumentException
    if (i == 0)
      Nil
    else {
      var (ls, x) = removeAt(Random.nextInt(list.length), list)
      x :: randomSelect(i - 1, ls)
    }
  }
}
