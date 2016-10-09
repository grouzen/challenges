object P08 {

  def compress[A](list: List[A]): List[A] = list match {
    case p :: q :: tail if p == q => compress(tail)
    case head :: tail => head :: compress(tail)
    case Nil => Nil
  }

}
