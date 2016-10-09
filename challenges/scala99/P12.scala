object P12 {

  def decode[A](list: List[(Int, A)]): List[A] = list match {
    case (n, e) :: tail => List.fill(n)(e) ++ decode(tail)
    case Nil => Nil
  }

}
