object P09 {

  def pack[A](list: List[A]): List[List[A]] = {
    
    def pp[A](list: List[A], acc: List[A]): List[List[A]] = list match {
      case e :: tail if acc.isEmpty => pp(tail, List(e))
      case e :: tail if e == acc.head => pp(tail, e :: acc)
      case e :: tail => acc :: pp(tail, List(e))
      case Nil => List(acc)
    }

    pp(list, List())

  }

}
