package s_99

object Lists {
  // P01 (*) Find the last element of a list
  def last[A](list: List[A]): A = list match {
    case head :: Nil  => head
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }
  def last1[A](list: List[A]): A = list.last
}
