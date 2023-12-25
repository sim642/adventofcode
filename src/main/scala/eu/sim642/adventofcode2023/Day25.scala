package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents, GraphSearch, GraphTraversal, TargetNode, UnitNeighbors}

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

object Day25 {

  type Edge = (String, String)

  def connectedComponents(edges: Set[Edge]): collection.Set[collection.Set[String]] = {

    val forwardEdges = edges.groupMap(_._1)(_._2).withDefaultValue(Set.empty)
    val backwardEdges = edges.groupMap(_._2)(_._1).withDefaultValue(Set.empty)

    val graphComponents = new GraphComponents[String] {
      override def nodes: IterableOnce[String] = forwardEdges.keySet ++ backwardEdges.keySet

      override def unitNeighbors(node: String): IterableOnce[String] = forwardEdges(node) ++ backwardEdges(node)
    }

    BFS.components(graphComponents)
  }

  def disconnectComponents(edges: Set[Edge]): Int = {
    /* val foo = edges.toSeq.combinations(3)
      .map(edges -- _)
      .map(connectedComponents)
      .find(_.size == 2)
      .get
    foo.toSeq.map(_.size).product*/

    def edmondsKarp(startNode0: String, targetNode0: String): (Int, collection.Map[String, collection.Map[String, Int]]) = {

      val residual: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map.empty
      for ((u, v) <- edges) {
        // TODO: why doesn't withDefault work?
        if (!residual.contains(u))
          residual(u) = mutable.Map.empty
        if (!residual.contains(v))
          residual(v) = mutable.Map.empty
        residual(u)(v) = 1
        residual(v)(u) = 1
      }

      var flow = 0
      boundary {
        while (true) {
          val graphSearch = new GraphSearch[String] with UnitNeighbors[String] with TargetNode[String] {
            override val startNode: String = startNode0

            override def unitNeighbors(node: String): IterableOnce[String] = {
              for {
                (toNode, r) <- residual(node)
                if r > 0
              } yield toNode
            }

            override val targetNode: String = targetNode0
          }

          val r = BFS.searchPaths(graphSearch)
          if (r.target.isEmpty)
            break()
          val path = r.paths(r.target.get._1).reverse

          val newFlow = (path lazyZip path.tail).map((u, v) => residual(u)(v)).max
          flow += newFlow
          for ((u, v) <- path lazyZip path.tail) {
            residual(u)(v) -= newFlow
            residual(v)(u) += newFlow
          }
        }
      }

      //println(flow)

      (flow, residual)
    }

    val nodes = edges.flatMap(Seq(_, _))
    val startNode0 = nodes.head
    //val startNode0 = "frs"

    for (targetNode <- nodes - startNode0) {
      val (flow, residual) = edmondsKarp(startNode0, targetNode)
      if (flow == 3) {
        val graphTraversal = new GraphTraversal[String] with UnitNeighbors[String] {
          override val startNode: String = startNode0

          override def unitNeighbors(node: String): IterableOnce[String] = {
            for {
              (toNode, r) <- residual(node)
              if r > 0
            } yield toNode
          }
        }

        val comp = BFS.traverse(graphTraversal).nodes
        //println(comp)
        return comp.size * (nodes.size - comp.size)
      }
    }

    ???
  }

  def disconnectComponents2(edges: Set[Edge]): Int = {
    val foo = connectedComponents(edges -- Seq("bbg" -> "htb", "htb" -> "bbg", "pcc" -> "htj", "htj" -> "pcc", "pjj" -> "dlk", "dlk" -> "pjj"))
    println(foo.size)
    foo.toSeq.map(_.size).product
  }

  def parseEdges(input: String): Set[Edge] = {
    (for {
      case s"$from: $tos" <- input.linesIterator
      to <- tos.split(' ')
    } yield (from, to)).toSet
  }

  def printDot(edges: Set[Edge]): Unit = {
    println("graph G {")
    for ((a, b) <- edges)
      println(s"  $a -- $b;")
    println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //printDot(parseEdges(input))
    // bbg -- htb
    // pcc -- htj
    // pjj -- dlk
    println(disconnectComponents2(parseEdges(input)))

    // part 1: 1468 - too low (had pjj wrong)
  }
}
