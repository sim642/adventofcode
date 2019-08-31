package eu.sim642.adventofcode2015

object Day9 {

  // TODO: move TSP implementations to library

  // copied & modified from 2016 Day 24
  def tsp[A](distMatrix: Map[A, Map[A, Int]]): Int = {
    distMatrix.keySet.toVector
      .permutations
      .map({ path =>
        path.zip(path.tail)
          .map({ case (from, to) => distMatrix(from)(to) })
          .sum
      }).min
  }

  def shortestRoute(input: String): Int = tsp(parseDistMatrix(input))

  // copied & modified from 2016 Day 24
  def tspLongest[A](distMatrix: Map[A, Map[A, Int]]): Int = {
    distMatrix.keySet.toVector
      .permutations
      .map({ path =>
        path.zip(path.tail)
          .map({ case (from, to) => distMatrix(from)(to) })
          .sum
      }).max
  }

  def longestRoute(input: String): Int = tspLongest(parseDistMatrix(input))


  private val edgeRegex = """(\w+) to (\w+) = (\d+)""".r

  def parseEdge(s: String): (String, Int, String) = s match {
    case edgeRegex(from, to, dist) => (from, dist.toInt, to)
  }

  def parseDistMatrix(input: String): Map[String, Map[String, Int]] = {
    input.linesIterator.map(parseEdge).foldLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty[String, Int]))({ case (distMatrix, (from, dist, to)) =>
      distMatrix + (from -> (distMatrix(from) + (to -> dist))) + (to -> (distMatrix(to) + (from -> dist)))
    })
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(shortestRoute(input))
    println(longestRoute(input))
  }
}
