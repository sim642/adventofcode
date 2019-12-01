package eu.sim642.adventofcode2019

object Day1 {

  def requiredFuel(mass: Int): Int = mass / 3 - 2

  def totalRequiredFuel(masses: Seq[Int]): Int = masses.map(requiredFuel).sum

  def parseMasses(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalRequiredFuel(parseMasses(input)))
  }
}
