object Solution {
    
  import scala.io.StdIn.{readInt, readLine}

  // TODO:
  //  - calculate a depth of the tree
  //  - swap op
  //  - get nodes on depth N
  //  - inorder traversal

  // sealed trait Tree
  // case class Node(left: Option[Tree], right: Option[Tree], value: Int) extends Tree
  // case class Leaf(value: Int) extends Tree

  case class Tree(left: Option[Tree] = None, value: Int, right: Option[Tree] = None)
  
  

  def makeTree(): Option[Tree] = {
    val nodes = List.range(0, readInt())
      .map(_ => readLine().split(" ").map(_.toInt) match {
        case Array(l: Int, r: Int) => (l, r)
      })
    
    println(nodes)

    Some(Tree(value = 1))
  }

  def main(args: Array[String]) {
    val tree = makeTree()
    
  }
}
