package eu.sim642.adventofcodelib.graph

import scala.collection.mutable

object Dijkstra {
  // TODO: reduce duplication without impacting performance

  // copied from AStar
  def traverse[A](graphTraversal: GraphTraversal[A]): Distances[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(node: A, dist: Int): Unit = {
      toVisit.enqueue((dist, node))
    }

    enqueue(graphTraversal.startNode, 0)

    while (toVisit.nonEmpty) {
      val (dist, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node)) {
        visitedDistance(node) = dist

        def goNeighbor(newNode: A, distDelta: Int): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + distDelta
            enqueue(newNode, newDist)
          }
        }

        graphTraversal.neighbors(node).iterator.foreach(goNeighbor.tupled)
      }
    }

    new Distances[A] {
      override def distances: collection.Map[A, Int] = visitedDistance
    }
  }

  // copied from AStar
  def search[A](graphSearch: GraphSearch[A]): Distances[A] & Target[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(node: A, dist: Int): Unit = {
      toVisit.enqueue((dist, node))
    }

    enqueue(graphSearch.startNode, 0)

    while (toVisit.nonEmpty) {
      val (dist, node) = toVisit.dequeue()
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
            enqueue(newNode, newDist)
          }
        }

        graphSearch.neighbors(node).iterator.foreach(goNeighbor.tupled)
      }
    }

    new Distances[A] with Target[A] {
      override def distances: collection.Map[A, Int] = visitedDistance

      override def target: Option[(A, Int)] = None
    }
  }

  // copied from search, modified like BFS.searchPaths
  def searchAllPaths[A](graphSearch: GraphSearch[A]): Distances[A] & AllPaths[A] & Target[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val prevNode: mutable.Map[A, mutable.Set[A]] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, Option[A], A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueue(oldNode: Option[A], node: A, dist: Int): Unit = {
      toVisit.enqueue((dist, oldNode, node))
    }

    enqueue(None, graphSearch.startNode, 0)

    while (toVisit.nonEmpty) {
      val (dist, oldNode, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node)) {
        visitedDistance(node) = dist
        for (oldNode <- oldNode)
          prevNode(node) = mutable.Set(oldNode)

        if (graphSearch.isTargetNode(node, dist)) {
          return new Distances[A] with AllPaths[A] with Target[A] {
            override def distances: collection.Map[A, Int] = visitedDistance

            override def allPrevNodes: collection.Map[A, collection.Set[A]] = prevNode

            override def target: Option[(A, Int)] = Some(node -> dist)
          }
        }


        def goNeighbor(newNode: A, distDelta: Int): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + distDelta
            enqueue(Some(node), newNode, newDist)
          }
        }

        graphSearch.neighbors(node).iterator.foreach(goNeighbor.tupled)
      }
      else { // visitedDistance.contains(node)
        for (oldNode <- oldNode if visitedDistance(node) == dist)
          prevNode(node) += oldNode
      }
    }

    new Distances[A] with AllPaths[A] with Target[A] {
      override def distances: collection.Map[A, Int] = visitedDistance

      override def allPrevNodes: collection.Map[A, collection.Set[A]] = prevNode

      override def target: Option[(A, Int)] = None
    }
  }
}
