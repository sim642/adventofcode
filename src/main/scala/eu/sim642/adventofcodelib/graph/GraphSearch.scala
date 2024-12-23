package eu.sim642.adventofcodelib.graph

trait GraphSearch0[A] extends GraphTraversal0[A] {
  //def isTargetNode(node: A): Boolean
  def isTargetNode(node: A, dist: Int): Boolean // TODO: does dist-based target make sense for A*?
}

trait GraphSearch[A] extends GraphTraversal[A], GraphSearch0[A]

trait TargetNode[A] { this: GraphSearch[A] =>
  val targetNode: A

  override def isTargetNode(node: A, dist: Int): Boolean = node == targetNode
}

trait Heuristic[A] { this: GraphSearch[A] =>
  def heuristic(node: A): Int
}


trait Target[A] {
  def target: Option[(A, Int)]
}
