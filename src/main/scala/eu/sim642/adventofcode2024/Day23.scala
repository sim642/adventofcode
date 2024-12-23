package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.graph.BronKerbosch

object Day23 {

  type Computer = String
  type Edge = (Computer, Computer)

  def edges2neighbors(edges: Set[Edge]): Map[Computer, Set[Computer]] =
    (edges ++ edges.map(_.swap)).groupMap(_._1)(_._2)

  def find3Cliques(edges: Set[Edge]): Set[Set[Computer]] = {
    val neighbors = edges2neighbors(edges)
    for {
      (from, to) <- edges
      third <- neighbors(from) & neighbors(to)
    } yield Set(from, third, to)
  }

  def count3CliquesT(edges: Set[Edge]): Int = find3Cliques(edges).count(_.exists(_.startsWith("t")))

  def maximumClique(edges: Set[Edge]): Set[Computer] =
    BronKerbosch.maximumClique(edges2neighbors(edges))

  def lanPartyPassword(edges: Set[Edge]): String =
    maximumClique(edges).toSeq.sorted.mkString(",")

  def parseEdge(s: String): Edge = s match {
    case s"$from-$to" => (from, to)
  }

  def parseEdges(input: String): Set[Edge] = input.linesIterator.map(parseEdge).toSet

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(count3CliquesT(parseEdges(input)))
    println(lanPartyPassword(parseEdges(input)))

    // part 1: 2366 - too high (used contains 't' instead of startsWith 't')
  }
}
