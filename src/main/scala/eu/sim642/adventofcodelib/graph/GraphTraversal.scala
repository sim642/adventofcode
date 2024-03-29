package eu.sim642.adventofcodelib.graph

import eu.sim642.adventofcodelib.LazyListImplicits._

trait GraphTraversal[A] {
  val startNode: A
  def neighbors(node: A): IterableOnce[(A, Int)]
}

trait UnitNeighbors[A] { this: GraphTraversal[A] =>
  def unitNeighbors(node: A): IterableOnce[A]

  override final def neighbors(node: A): IterableOnce[(A, Int)] = unitNeighbors(node).iterator.map(_ -> 1)
}


trait Distances[A] {
  def distances: collection.Map[A, Int]

  def nodes: collection.Set[A] = distances.keySet
}

trait Paths[A] {
  def prevNodes: collection.Map[A, A]

  def paths: collection.Map[A, Seq[A]] = {
    prevNodes.map((node, _) =>
      node -> (node +: LazyList.unfold0(node)(prevNodes.get)).reverse
    )
  }
}

trait Order[A] {
  def nodeOrder: collection.Seq[A]
}
