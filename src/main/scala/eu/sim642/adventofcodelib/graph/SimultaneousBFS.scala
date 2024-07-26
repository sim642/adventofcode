package eu.sim642.adventofcodelib.graph

import scala.annotation.tailrec

/**
 * BFS, which performs all steps simultaneously instead of one-by-one from a queue.
 * This gives additional node distances in search results.
 */
object SimultaneousBFS {
  // TODO: reduce duplication without impacting performance

  // moved from 2018 Day 20
  def traverse[A](graphTraversal: GraphTraversal[A] with UnitNeighbors[A]): Distances[A] = {

    @tailrec
    def helper(visited: Map[A, Int], toVisit: Map[A, Int]): Distances[A] = {
      // TODO: use one dist: Int argument instead of all same toVisit values
      val newToVisit = for {
        (node, dist) <- toVisit
        newNode <- graphTraversal.unitNeighbors(node).iterator
        if !visited.contains(newNode)
      } yield newNode -> (dist + 1)
      val newVisited = visited ++ toVisit
      if (newToVisit.isEmpty) {
        new Distances[A] {
          override def distances: collection.Map[A, Int] = newVisited
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
      val newVisited = visited ++ toVisit
      toVisit.find(graphSearch.isTargetNode.tupled) match {
        case targetNodeDist@Some(_) =>
          new Distances[A] with Target[A] {
            override def distances: collection.Map[A, Int] = newVisited

            override def target: Option[(A, Int)] = targetNodeDist
          }
        case None =>
          val newToVisit = for {
            (node, dist) <- toVisit
            newNode <- graphSearch.unitNeighbors(node).iterator
            if !visited.contains(newNode)
          } yield newNode -> (dist + 1)
          if (newToVisit.isEmpty) {
            new Distances[A] with Target[A] {
              override def distances: collection.Map[A, Int] = newVisited

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
