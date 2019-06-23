package eu.sim642.adventofcode2016

object Day3 {

  def isValidTriangle(a: Int, b: Int, c: Int): Boolean = {
    a + b > c && a + c > b && b + c > a
  }

  def countPossibleTriangles(triangles: Seq[(Int, Int, Int)]): Int = triangles.count({ case (a, b, c) => isValidTriangle(a, b, c)})

  def parseInput(input: String): Seq[(Int, Int, Int)] = input.lines.map({ line =>
    val parts = line.trim.split(" +").map(_.toInt)
    (parts(0), parts(1), parts(2))
  }).toSeq

  def countPossibleTriangles(input: String): Int = countPossibleTriangles(parseInput(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPossibleTriangles(input))
  }
}
