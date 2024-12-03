package eu.sim642.adventofcode2024

object Day3 {

  private val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r

  def sumUncorruptedMuls(s: String): Int = {
    mulRegex.findAllMatchIn(s)
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumUncorruptedMuls(input))
  }
}
