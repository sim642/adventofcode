package eu.sim642.adventofcode2015

object Day12 {

  private val numberRegex = """-?\d+""".r

  def sumNumbers(input: String): Int = {
    numberRegex.findAllIn(input)
      .map(_.toInt)
      .sum
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumNumbers(input))
  }
}
