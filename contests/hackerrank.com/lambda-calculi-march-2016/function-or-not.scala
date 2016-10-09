object Solution {
  
  import scala.io.StdIn.{readInt, readLine}

  def main(args: Array[String]) {
    val t = readInt()
    
    for(i <- List.range(0, t)) {
      if(validate(readFn()))
        println("YES")
      else
        println("NO")
    }
  }

  def readFn(): List[(Int, Int)] = {
    val n = readInt()
    
    for(i <- List.range(0, n)) yield {
      readLine().split(" ").map(_.toInt) match {
        case Array(x: Int, y: Int) => (x, y)
      }
    }
  }
    
  def validate(fn: List[(Int, Int)]): Boolean = {
    val mem = scala.collection.mutable.HashMap.empty[Int, Int]

    for((x, y) <- fn) {
      mem.get(x) match {
        case None => mem += (x -> y)
        case Some(yy) if yy != y => return false
      }
    }

    return true
  }

}
