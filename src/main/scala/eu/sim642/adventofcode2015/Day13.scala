package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.graph.NaiveTSP

object Day13 {

  // modified from 2015 Day 9
  def optimalHappiness(input: String): Int = NaiveTSP.cycleLength(parseDistMatrix(input))(Ordering.Int.reverse)


  private val edgeRegex = """(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.""".r

  def parseEdge(s: String): (String, Int, String) = s match {
    case edgeRegex(from, gainLose, amount, to) =>
      val dist = amount.toInt * (gainLose match {
        case "gain" => 1
        case "lose" => -1
      })
      (from, dist, to)
  }

  def parseDistMatrix(input: String): Map[String, Map[String, Int]] = {
    input.linesIterator.map(parseEdge).foldLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty[String, Int].withDefaultValue(0)))({ case (distMatrix, (from, dist, to)) =>
      distMatrix +
        (from -> (distMatrix(from) + (to -> (distMatrix(from)(to) + dist)))) +
        (to -> (distMatrix(to) + (from -> (distMatrix(to)(from) + dist))))
    })
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(optimalHappiness(input))
  }
}
