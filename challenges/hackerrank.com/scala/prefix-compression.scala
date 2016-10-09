object Solution {
  import scala.io.StdIn.{readInt, readLine}

  def main(args: Array[String]) {
    val s1 = readLine()
    val s2 = readLine()

    val l = s1.zip(s2)
      .zipWithIndex
      .filter { case ((p, q), _) => p == q }
      .last
      ._2 + 1
    
    println(f"${l} ${s1.substring(0, l)}")
    println(f"${s1.length - l} ${s1.substring(l)}")
    println(f"${s2.length - l} ${s2.substring(l)}")
  }

}
