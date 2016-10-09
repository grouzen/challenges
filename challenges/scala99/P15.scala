object P15 {

  def duplicateN[A](n: Int, list: List[A]): List[A] = list match {
    case e :: tail => List.fill(n)(e) ++ duplicateN(n, tail)
    case Nil => Nil
  }

}
