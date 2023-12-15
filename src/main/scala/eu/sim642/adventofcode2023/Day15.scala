package eu.sim642.adventofcode2023

object Day15 {

  def hash(s: String): Int = s.foldLeft(0)((acc, c) => (17 * (acc + c.toInt)) % 256)

  def sumStepHashes(steps: Seq[String]): Int = steps.map(hash).sum


  def parseSteps(input: String): Seq[String] = input.split(',').toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumStepHashes(parseSteps(input)))
  }
}
