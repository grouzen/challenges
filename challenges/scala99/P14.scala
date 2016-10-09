object P14 {

  def duplicate[A](list: List[A]): List[A] = list match {
    case e :: tail => e :: e :: duplicate(tail)
    case Nil => Nil
  }

}
