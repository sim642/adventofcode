package eu.sim642.adventofcodelib.graph

trait GraphTraversal[A] {
  val startNode: A
  def neighbors(node: A): TraversableOnce[(A, Int)]
}

trait UnitNeighbors[A] { this: GraphTraversal[A] =>
  def unitNeighbors(node: A): TraversableOnce[A]

  override final def neighbors(node: A): TraversableOnce[(A, Int)] = unitNeighbors(node).map(_ -> 1)
}


trait Distances[A] {
  def distances: Map[A, Int]

  def nodes: Set[A] = distances.keySet
}
