package eu.sim642.adventofcodelib

import scala.annotation.tailrec
import scala.collection.mutable

trait GraphSearch[A] {
  val startNode: A
  def neighbors(node: A): TraversableOnce[(A, Int)]
  //def isTargetNode(node: A): Boolean
  def isTargetNode(node: A, dist: Int): Boolean // TODO: does dist-based target make sense for A*?
}

// TODO: inheritance should be other way around?
trait GraphTraversal[A] extends GraphSearch[A] {
  override def isTargetNode(node: A, dist: Int): Boolean = false
}

trait UnitNeighbors[A] { this: GraphSearch[A] =>
  def unitNeighbors(node: A): TraversableOnce[A]

  override final def neighbors(node: A): TraversableOnce[(A, Int)] = unitNeighbors(node).map(_ -> 1)
}

trait TargetNode[A] { this: GraphSearch[A] =>
  val targetNode: A

  override def isTargetNode(node: A, dist: Int): Boolean = node == targetNode
}

trait Heuristic[A] { this: GraphSearch[A] =>
  def heuristic(node: A): Int
}


trait Distances[A] {
  def distances: Map[A, Int]

  def nodes: Set[A] = distances.keySet
}

trait Target[A] {
  def target: Option[(A, Int)]
}

object GraphSearch {
  // moved from 2018 Day 20
  def bfs[A](graphSearch: GraphSearch[A] with UnitNeighbors[A]): Distances[A] with Target[A] = {

    @tailrec
    def helper(visited: Map[A, Int], toVisit: Map[A, Int]): Distances[A] with Target[A] = {
      // TODO: use one dist: Int argument instead of all same toVisit values
      val neighbors = for {
        (node, dist) <- toVisit
        newNode <- graphSearch.unitNeighbors(node)
      } yield newNode -> (dist + 1)
      val newVisited = visited ++ toVisit
      // TODO: optimized version of BFS for just traversal
      toVisit.find((graphSearch.isTargetNode _).tupled) match {
        case targetNodeDist@Some(_) =>
          new Distances[A] with Target[A] {
            override def distances: Map[A, Int] = newVisited

            override def target: Option[(A, Int)] = targetNodeDist
          }
        case None =>
          //val newToVisit = neighbors -- visited.keys
          val newToVisit = neighbors.filterKeys(!visited.contains(_)) // more efficient than -- because visited is large
          if (newToVisit.isEmpty) {
            new Distances[A] with Target[A] {
              override def distances: Map[A, Int] = newVisited

              override def target: Option[(A, Int)] = None
            }
          }
          else
            helper(newVisited, newToVisit)
      }
    }

    helper(Map.empty, Map(graphSearch.startNode -> 0))
  }

  // moved from 2018 Day 22
  def aStar[A](graphSearch: GraphSearch[A] with Heuristic[A]): Distances[A] with Target[A] = {
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
            override def distances: Map[A, Int] = visitedDistance.toMap // TODO: don't copy entire mutable.Map

            override def target: Option[(A, Int)] = Some(node -> dist)
          }
        }


        def goNeighbor(newNode: A, distDelta: Int): Unit = {
          if (!visitedDistance.contains(newNode)) { // avoids some unnecessary queue duplication but not all
            val newDist = dist + distDelta
            enqueueHeuristically(newNode, newDist)
          }
        }

        graphSearch.neighbors(node).foreach((goNeighbor _).tupled) // eta expansion postfix operator _
      }
    }

    new Distances[A] with Target[A] {
      override def distances: Map[A, Int] = visitedDistance.toMap // TODO: don't copy entire mutable.Map

      override def target: Option[(A, Int)] = None
    }
  }
}
