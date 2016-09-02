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
}
