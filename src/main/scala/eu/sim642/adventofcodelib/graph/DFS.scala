package eu.sim642.adventofcodelib.graph

import scala.collection.mutable

object DFS {
  // TODO: reduce duplication without impacting performance

  // copied from BFS
  def traverse[A](graphTraversal: GraphTraversal[A] with UnitNeighbors[A]): Distances[A] with Order[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val visitedOrder: mutable.Buffer[A] = mutable.Buffer.empty
    val toVisit: mutable.Stack[(Int, A)] = mutable.Stack.empty

    def enqueue(node: A, dist: Int): Unit = {
      toVisit.push((dist, node))
    }

    enqueue(graphTraversal.startNode, 0)

    while (toVisit.nonEmpty) {
      val (dist, node) = toVisit.pop()
      if (!visitedDistance.contains(node)) {
        visitedDistance(node) = dist
        visitedOrder += node

        def goNeighbor(newNode: A): Unit = {
          // cannot avoid unnecessary stack duplication here: https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html
          val newDist = dist + 1
          enqueue(newNode, newDist)
        }

        graphTraversal.unitNeighbors(node).iterator.foreach(goNeighbor)
      }
    }

    new Distances[A] with Order[A] {
      override def distances: collection.Map[A, Int] = visitedDistance

      override def nodeOrder: collection.Seq[A] = visitedOrder
    }
  }
}
