package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.graph.*

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

object Day25 {

  type Edge = (String, String)

  def edmondsKarp(edges: Set[Edge], startNode0: String, targetNode0: String): (Int, collection.Map[String, collection.Map[String, Int]]) = {

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

    var maxFlow = 0

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

        val searchResult = BFS.searchPaths(graphSearch)
        if (searchResult.target.isEmpty)
          break()
        else {
          val targetPath = searchResult.paths(targetNode0).reverse // TODO: reverse in library?
          val pathEdges = targetPath lazyZip targetPath.tail
          val pathFlow = pathEdges.map(residual(_)(_)).min
          for ((u, v) <- pathEdges) {
            residual(u)(v) -= pathFlow
            residual(v)(u) += pathFlow
          }
          maxFlow += pathFlow
        }
      }
    }

    (maxFlow, residual)
  }

  def disconnectComponents(edges: Set[Edge]): Int = {
    val nodes = edges.flatMap(Seq(_, _))
    val startNode0 = nodes.head

    val (maxFlow, residual) =
      (nodes - startNode0).view
        .map(edmondsKarp(edges, startNode0, _))
        .find(_._1 == 3).get

    val graphTraversal = new GraphTraversal[String] with UnitNeighbors[String] {
      override val startNode: String = startNode0

      override def unitNeighbors(node: String): IterableOnce[String] = {
        for {
          (toNode, r) <- residual(node)
          if r > 0
        } yield toNode
      }
    }

    val component = BFS.traverse(graphTraversal).nodes
    val size1 = component.size
    val size2 = nodes.size - size1
    size1 * size2
  }


  def parseEdges(input: String): Set[Edge] = {
    (for {
      case s"$from: $tos" <- input.linesIterator
      to <- tos.split(' ')
    } yield (from, to)).toSet
  }

  def printEdgesDot(edges: Set[Edge]): Unit = {
    println("graph edges {")
    for ((from, to) <- edges)
      println(s"  $from -- $to;")
    println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //printEdgesDot(parseEdges(input))
    // by manual inspection:
    // bbg -- htb
    // pcc -- htj
    // pjj -- dlk
    println(disconnectComponents(parseEdges(input)))

    // part 1: 1468 - too low (had pjj wrong)
  }
}
