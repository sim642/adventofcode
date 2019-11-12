package eu.sim642.adventofcodelib.graph

import scala.collection.mutable

object AStar {
  // moved from 2018 Day 22
  def search[A](graphSearch: GraphSearch[A] with Heuristic[A]): Distances[A] with Target[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueueHeuristically(node: A, dist: Int): Unit = {
      toVisit.enqueue((dist + graphSearch.heuristic(node), dist, node))
    }

    enqueueHeuristically(graphSearch.startNode, 0)

    while (toVisit.nonEmpty) {
      val (_, dist, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node)) {
        visitedDistance(node) = dist

        if (graphSearch.isTargetNode(node, dist)) {
          return new Distances[A] with Target[A] {
            override def distances: collection.Map[A, Int] = visitedDistance

            override def target: Option[(A, Int)] = Some(node -> dist)
          }
        }


        def goNeighbor(newNode: A, distDelta: Int): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + distDelta
            enqueueHeuristically(newNode, newDist)
          }
        }

        graphSearch.neighbors(node).iterator.foreach((goNeighbor _).tupled) // eta expansion postfix operator _
      }
    }

    new Distances[A] with Target[A] {
      override def distances: collection.Map[A, Int] = visitedDistance

      override def target: Option[(A, Int)] = None
    }
  }
}
