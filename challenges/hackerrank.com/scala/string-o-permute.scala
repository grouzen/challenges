object Solution {
  import scala.io.StdIn.{readInt, readLine}

  def main(args: Array[String]) {
    val t = readInt()
    
    for(i <- 0 until t) {
      println(readLine()
        .grouped(2)
        .map(s => s(1).toString ++ s(0).toString)
        .toList
        .foldLeft("")(_ ++ _))
    }

  }

}
