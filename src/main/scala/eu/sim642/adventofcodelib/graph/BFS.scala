package eu.sim642.adventofcodelib.graph

import scala.annotation.tailrec
import scala.collection.mutable

object BFS {
  // TODO: reduce duplication without impacting performance

  // copied from Dijkstra
  def traverse[A](graphTraversal: GraphTraversal[A] with UnitNeighbors[A]): Distances[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)] = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit = {
      toVisit.enqueue((dist, node))
    }

    enqueue(graphTraversal.startNode, 0)

    while (toVisit.nonEmpty) {
      val (dist, node) = toVisit.dequeue()
      if (!visitedDistance.contains(node)) {
        visitedDistance(node) = dist

        def goNeighbor(newNode: A): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + 1
            enqueue(newNode, newDist)
          }
        }

        graphTraversal.unitNeighbors(node).iterator.foreach(goNeighbor)
      }
    }

    new Distances[A] {
      override def distances: collection.Map[A, Int] = visitedDistance
    }
  }

  // copied from Dijkstra
  def search[A](graphSearch: GraphSearch[A] with UnitNeighbors[A]): Distances[A] with Target[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)] = mutable.Queue.empty

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


        def goNeighbor(newNode: A): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + 1
            enqueue(newNode, newDist)
          }
        }

        graphSearch.unitNeighbors(node).iterator.foreach(goNeighbor)
      }
    }

    new Distances[A] with Target[A] {
      override def distances: collection.Map[A, Int] = visitedDistance

      override def target: Option[(A, Int)] = None
    }
  }

  // moved from 2017 Day 14
  def components[A](graphComponents: GraphComponents[A]): collection.Set[collection.Set[A]] = {

    def bfs(start: A): collection.Set[A] = {

      val graphTraversal = new GraphTraversal[A] with UnitNeighbors[A] {
        override val startNode: A = start

        override def unitNeighbors(node: A): IterableOnce[A] = graphComponents.unitNeighbors(node)
      }

      BFS.traverse(graphTraversal).nodes
    }

    def bfsGroups(nodes: Set[A]): Set[collection.Set[A]] = {
      if (nodes.isEmpty)
        Set.empty
      else {
        val startNode = nodes.head // take any node
        val group = bfs(startNode) // find its component
        val restNodes = nodes -- group
        bfsGroups(restNodes) + group
      }
    }

    bfsGroups(graphComponents.nodes.iterator.toSet)
  }
}
