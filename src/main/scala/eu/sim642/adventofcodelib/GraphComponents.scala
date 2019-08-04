package eu.sim642.adventofcodelib

// TODO: common interface with GraphSearch
trait FiniteGraph[A] {
  def nodes: TraversableOnce[A] // TODO: make this val and Set[A]?
  def unitNeighbors(node: A): TraversableOnce[A]
}

object GraphComponents {
  // moved from 2017 Day 14
  def bfs[A](finiteGraph: FiniteGraph[A]): Set[Set[A]] = {

    def bfs(start: A): Set[A] = {

      val graphTraversal = new GraphTraversal[A] with UnitNeighbors[A] {
        override val startNode: A = start

        override def unitNeighbors(node: A): TraversableOnce[A] = finiteGraph.unitNeighbors(node)
      }

      GraphSearch.bfs(graphTraversal).nodes
    }

    def bfsGroups(nodes: Set[A]): Set[Set[A]] = {
      if (nodes.isEmpty)
        Set.empty
      else {
        val startNode = nodes.head // take any node
        val group = bfs(startNode) // find its component
        val restNodes = nodes -- group
        bfsGroups(restNodes) + group
      }
    }

    bfsGroups(finiteGraph.nodes.toSet)
  }
}
