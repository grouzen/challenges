object P16 {

  def drop[A](n: Int, list: List[A]): List[A] = {
    
    def dd[A](m: Int, n: Int, list: List[A]): List[A] = list match {
      case Nil => Nil
      case e :: tail => n match {
        case 1  => 
          dd(m, m, tail)
        case nn => 
          e :: dd(m, nn - 1, tail)
      }
      
    }

    dd(n, n, list)

  }

}
