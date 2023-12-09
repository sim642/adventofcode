package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.graph.NaiveTSP

object Day9 {

  def shortestRoute(input: String): Int = NaiveTSP.pathLength(parseDistMatrix(input))

  def longestRoute(input: String): Int = NaiveTSP.pathLength(parseDistMatrix(input))(using Ordering.Int.reverse)


  private val edgeRegex = """(\w+) to (\w+) = (\d+)""".r

  def parseEdge(s: String): (String, Int, String) = s match {
    case edgeRegex(from, to, dist) => (from, dist.toInt, to)
  }

  def parseDistMatrix(input: String): Map[String, Map[String, Int]] = {
    input.linesIterator.map(parseEdge).foldLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty[String, Int]))({ case (distMatrix, (from, dist, to)) =>
      distMatrix +
        (from -> (distMatrix(from) + (to -> dist))) +
        (to -> (distMatrix(to) + (from -> dist)))
    })
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(shortestRoute(input))
    println(longestRoute(input))
  }
}
