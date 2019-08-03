package eu.sim642.adventofcodelib

import scala.collection.mutable
import scala.util.control.Breaks._

trait GraphSearch[A] {
  def startNodes: TraversableOnce[A]
  def neighbors(node: A): TraversableOnce[(A, Int)]
  def isTargetNode(node: A): Boolean
}

trait UnitNeighbors[A] { this: GraphSearch[A] =>
  def unitNeighbors(node: A): TraversableOnce[A]

  override def neighbors(node: A): TraversableOnce[(A, Int)] = unitNeighbors(node).map(_ -> 1)
}

trait TargetNode[A] { this: GraphSearch[A] =>
  val targetNode: A

  override def isTargetNode(node: A): Boolean = node == targetNode
}

trait Heuristic[A] { this: GraphSearch[A] =>
  def heuristic(node: A): Int
}


trait Distances[A] {
  def distances: Map[A, Int]
}

trait Target[A] {
  def target: Option[(A, Int)]
}

object GraphSearch {
  // moved from 2018 Day 22
  def aStar[A](graphSearch: GraphSearch[A] with Heuristic[A]): Distances[A] with Target[A] = {
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    def enqueueHeuristically(node: A, dist: Int): Unit = {
      toVisit.enqueue((dist + graphSearch.heuristic(node), dist, node))
    }

    graphSearch.startNodes.foreach(enqueueHeuristically(_, 0))

    breakable {
      while (toVisit.nonEmpty) {
        val (_, dist, node) = toVisit.dequeue()
        if (!visitedDistance.contains(node)) {
          visitedDistance(node) = dist

          if (graphSearch.isTargetNode(node)) {
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
    }

    new Distances[A] with Target[A] {
      override def distances: Map[A, Int] = visitedDistance.toMap // TODO: don't copy entire mutable.Map

      override def target: Option[(A, Int)] = None
    }
  }
}
