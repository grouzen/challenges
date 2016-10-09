object ValidBST {
  
  // parser/builder
  // preorder traversal
  // validator

  import scala.io.StdIn.{readInt, readLine}
  
  //case class Tree(left: Option[Tree], value: Int, right: Option[Tree])

  def main(args: Array[String]) = {
    for(i <- 0 until readInt()) {
      readInt() // in scala we don't need the value of this ;)
      println(solve(readLine().split(" ").map(_.toInt)))
    }
  }
  
  def solve(nodes: Array[Int]) {
    validate(nodes) match {
      case Some(tree) => println("YES")
      case None       => println("NO")
    }
  }

  def validate(nodes: Array[Int]): Boolean = {
    go(nodes: Array[Int], parent: Int, prev: Array[Int]): Boolean = 
      nodes match {
        // build right node
        case Array(head, tail@_*) if head > parent =
          prev.zipWithIndex.max
          // Ugly imperative shit
          if(tail(0) < parent) {
            None
          } else {
            // rollback on max(prev)
            if(head > parent) {
              
            }
            val (left, rest) = go(tail, head, prev)
          }
        // build left node
        case Array(head, tail@_*) =   
            Some(Tree(value = head))
          
        case _ => true
      }

    go(nodes.tail, nodes(0), Array.empty[Int])
  }

}
