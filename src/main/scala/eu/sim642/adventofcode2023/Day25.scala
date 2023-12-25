package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents}

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
    val foo = edges.toSeq.combinations(3)
      .map(edges -- _)
      .map(connectedComponents)
      .find(_.size == 2)
      .get
    foo.toSeq.map(_.size).product
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
      println(s"  $a -> $b;")
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
