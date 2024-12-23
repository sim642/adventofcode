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

  def paths: PartialFunction[A, Seq[A]] =
    prevNodes.andThen(node =>
      (node #:: LazyList.unfold0(node)(prevNodes.get)).reverse // TODO: don't bother with LazyList now that it's a function
    )
}

trait AllPaths[A] { // does not extend Paths, because prevNodes is Map, not function
  def allPrevNodes: collection.Map[A, collection.Set[A]]
}

trait Order[A] {
  def nodeOrder: collection.Seq[A]
}
