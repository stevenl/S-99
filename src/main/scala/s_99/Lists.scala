package s_99

object Lists {
  // P01 (*) Find the last element of a list
  def last[A](list: List[A]): A = list match {
    case head :: Nil  => head
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }
  def last1[A](list: List[A]): A = list.last

  // P02 (*) Find the last but one element of a list
  def penultimate[A](list: List[A]): A = list match {
    case head :: last :: Nil => head
    case head :: tail => penultimate(tail)
    case Nil | _ :: Nil => throw new NoSuchElementException
  }
  def penultimate1[A](list: List[A]): A = {
    if (list.length > 1) list(list.length - 2)
    else throw new NoSuchElementException
  }
  def penultimate2[A](list: List[A]): A = {
    if (!list.isEmpty) list.init.last
    else throw new NoSuchElementException
  }

  // P03 (*) Find the Kth element of a list.
  def nth[A](n: Int, list: List[A]): A = {
    if (list.isEmpty || n < 0) throw new NoSuchElementException
    else if (n == 0) list.head
    else nth(n -1, list.tail)
  }
  def nth1[A](n: Int, list: List[A]): A = {
    if (0 <= n && n < list.length) list(n)
    else throw new NoSuchElementException
  }
}
