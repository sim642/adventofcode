package eu.sim642.adventofcodelib.graph

import scala.annotation.tailrec

object BFS {
  // TODO: reduce duplication without impacting performance

  // moved from 2018 Day 20
  def traverse[A](graphTraversal: GraphTraversal[A] with UnitNeighbors[A]): Distances[A] = {

    @tailrec
    def helper(visited: Map[A, Int], toVisit: Map[A, Int]): Distances[A] = {
      // TODO: use one dist: Int argument instead of all same toVisit values
      val neighbors = for {
        (node, dist) <- toVisit
        newNode <- graphTraversal.unitNeighbors(node)
      } yield newNode -> (dist + 1)
      val newVisited = visited ++ toVisit
      //val newToVisit = neighbors -- visited.keys
      val newToVisit = neighbors.filter({ case (node, _) => !visited.contains(node) }) // more efficient than -- because visited is large
      // filter instead of filterKeys because filterKeys is lazy and recomputes everything...
      // TODO: undo in Scala 2.13
      if (newToVisit.isEmpty) {
        new Distances[A] {
          override def distances: Map[A, Int] = newVisited
        }
      }
      else
        helper(newVisited, newToVisit)
    }

    helper(Map.empty, Map(graphTraversal.startNode -> 0))
  }

  // moved from 2018 Day 20
  def search[A](graphSearch: GraphSearch[A] with UnitNeighbors[A]): Distances[A] with Target[A] = {

    @tailrec
    def helper(visited: Map[A, Int], toVisit: Map[A, Int]): Distances[A] with Target[A] = {
      // TODO: use one dist: Int argument instead of all same toVisit values
      val neighbors = for {
        (node, dist) <- toVisit
        newNode <- graphSearch.unitNeighbors(node)
      } yield newNode -> (dist + 1)
      val newVisited = visited ++ toVisit
      toVisit.find((graphSearch.isTargetNode _).tupled) match {
        case targetNodeDist@Some(_) =>
          new Distances[A] with Target[A] {
            override def distances: Map[A, Int] = newVisited

            override def target: Option[(A, Int)] = targetNodeDist
          }
        case None =>
          //val newToVisit = neighbors -- visited.keys
          val newToVisit = neighbors.filter({ case (node, _) => !visited.contains(node) }) // more efficient than -- because visited is large
          // filter instead of filterKeys because filterKeys is lazy and recomputes everything...
          // TODO: undo in Scala 2.13
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

  // moved from 2017 Day 14
  def components[A](graphComponents: GraphComponents[A]): Set[Set[A]] = {

    def bfs(start: A): Set[A] = {

      val graphTraversal = new GraphTraversal[A] with UnitNeighbors[A] {
        override val startNode: A = start

        override def unitNeighbors(node: A): TraversableOnce[A] = graphComponents.unitNeighbors(node)
      }

      BFS.traverse(graphTraversal).nodes
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

    bfsGroups(graphComponents.nodes.toSet)
  }
}
