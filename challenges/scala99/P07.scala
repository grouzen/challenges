object P07 {

  def flatten(list: List[Any]): List[Any] = list match {
    case head :: tail if head.isInstanceOf[List[_]] => flatten(head.asInstanceOf[List[Any]]) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
    case Nil => Nil
  }

}
