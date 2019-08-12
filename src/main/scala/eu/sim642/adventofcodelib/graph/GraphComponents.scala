package eu.sim642.adventofcodelib.graph

// TODO: common interface with GraphSearch
trait GraphComponents[A] {
  def nodes: IterableOnce[A] // TODO: make this val and Set[A]?
  def unitNeighbors(node: A): IterableOnce[A]
}
