package eu.sim642.adventofcode2024

object Day23 {

  type Computer = String
  type Edge = (Computer, Computer)

  def find3Cliques(edges: Set[Edge]): Set[Set[Computer]] = {
    val neighbors = (edges ++ edges.map(_.swap)).groupMap(_._1)(_._2)

    for {
      (from, to) <- edges
      third <- neighbors(from) & neighbors(to)
    } yield Set(from, third, to)
  }

  def count3CliquesT(edges: Set[Edge]): Int = find3Cliques(edges).count(_.exists(_.startsWith("t")))

  // copied from 2018 day 23
  // TODO: move to library
  def maximumClique(neighbors: Map[Computer, Set[Computer]]): Set[Computer] = {
    var best: Set[Computer] = Set.empty

    def bronKerbosh(r: Set[Computer], p: Set[Computer], x: Set[Computer]): Unit = {
      if (p.isEmpty && x.isEmpty) {
        //println(r)
        if (r.size > best.size)
          best = r
      }
      else {
        //val u = p.headOption.getOrElse(x.head)
        val u = (p ++ x).maxBy(neighbors(_).size) // pivot on highest degree
        var p2 = p
        var x2 = x
        for (v <- p -- neighbors(u)) {
          bronKerbosh(r + v, p2 intersect neighbors(v), x2 intersect neighbors(v))
          p2 -= v
          x2 += v
        }
      }
    }

    bronKerbosh(Set.empty, neighbors.keySet, Set.empty)
    best
  }

  def maximumClique(edges: Set[Edge]): Set[Computer] = {
    val neighbors = (edges ++ edges.map(_.swap)).groupMap(_._1)(_._2)
    maximumClique(neighbors)
  }

  def lanPartyPassword(edges: Set[Edge]): String = {
    val clique = maximumClique(edges)
    clique.toSeq.sorted.mkString(",")
  }

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
